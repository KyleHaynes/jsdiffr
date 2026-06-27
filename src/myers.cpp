// Core Myers O(ND) diff algorithm, ported faithfully from jsdiff's
// src/diff/base.ts (the `Diff` base class).
//
// Tokens are pre-encoded to integers in R (so that token equality - including
// case-insensitivity, whitespace-insensitivity, custom comparators, etc. - is
// reduced to plain integer equality here). This C++ layer therefore only needs
// to run the edit-distance loop; tokenization, value reconstruction and all
// post-processing happen in R.
//
// We replicate jsdiff's linked-list-of-components representation with structural
// sharing (each path stores only its oldPos and the index of its last
// component; components point back to a `prev` component). At the end we walk
// the winning path's component list to produce the merged edit script as two
// integer vectors:
//   op    : 0 = common (unchanged), 1 = added, -1 = removed
//   count : number of tokens in that component
//
// Returning NULL signals that maxEditLength was exceeded or the timeout fired
// (matching jsdiff, which returns `undefined` in those cases).

#include <Rcpp.h>
#include <vector>
#include <climits>
#include <chrono>
using namespace Rcpp;

namespace {

struct Component {
  int count;
  int op;    // 0 = common, 1 = added, 2 = removed (mapped to -1 on output)
  int prev;  // index into the pool, or -1
};

struct Path {
  int oldPos;
  int lastIdx;
};

inline int push_component(std::vector<Component>& pool, int count, int op, int prev) {
  pool.push_back(Component{count, op, prev});
  return static_cast<int>(pool.size()) - 1;
}

// Mirrors jsdiff's Diff.prototype.addToPath. Consecutive components with the
// same op are merged by incrementing the count (sharing the merged component's
// predecessor), exactly as jsdiff does.
inline Path add_to_path(std::vector<Component>& pool, const Path& path, int op, int oldPosInc) {
  int lastIdx = path.lastIdx;
  if (lastIdx != -1 && pool[lastIdx].op == op) {
    int mergedCount = pool[lastIdx].count + 1;
    int mergedPrev = pool[lastIdx].prev;
    int idx = push_component(pool, mergedCount, op, mergedPrev);
    return Path{path.oldPos + oldPosInc, idx};
  } else {
    int idx = push_component(pool, 1, op, lastIdx);
    return Path{path.oldPos + oldPosInc, idx};
  }
}

// Mirrors jsdiff's Diff.prototype.extractCommon: greedily advance along matching
// tokens, recording a single common component for the run.
inline int extract_common(std::vector<Component>& pool, Path& path,
                          const int* a, const int* b, int oldLen, int newLen,
                          int diagonalPath) {
  int oldPos = path.oldPos;
  int newPos = oldPos - diagonalPath;
  int commonCount = 0;
  while (newPos + 1 < newLen && oldPos + 1 < oldLen && a[oldPos + 1] == b[newPos + 1]) {
    newPos++;
    oldPos++;
    commonCount++;
  }
  if (commonCount) {
    path.lastIdx = push_component(pool, commonCount, 0, path.lastIdx);
  }
  path.oldPos = oldPos;
  return newPos;
}

List build_output(const std::vector<Component>& pool, int lastIdx) {
  std::vector<int> ops, counts;
  int idx = lastIdx;
  while (idx != -1) {
    ops.push_back(pool[idx].op);
    counts.push_back(pool[idx].count);
    idx = pool[idx].prev;
  }
  int n = static_cast<int>(ops.size());
  IntegerVector op(n), count(n);
  for (int i = 0; i < n; i++) {
    int j = n - 1 - i;  // reverse: list was built tail-first
    int o = ops[j];
    op[i] = (o == 2) ? -1 : o;
    count[i] = counts[j];
  }
  return List::create(_["op"] = op, _["count"] = count);
}

} // namespace

// [[Rcpp::export]]
SEXP myers_diff_cpp(IntegerVector a, IntegerVector b,
                    double max_edit_length = 0, double timeout_ms = 0) {
  const int oldLen = a.size();
  const int newLen = b.size();
  const int* ap = (oldLen > 0) ? &a[0] : nullptr;
  const int* bp = (newLen > 0) ? &b[0] : nullptr;

  long long maxEditLength = static_cast<long long>(oldLen) + newLen;
  if (max_edit_length > 0 && static_cast<long long>(max_edit_length) < maxEditLength) {
    maxEditLength = static_cast<long long>(max_edit_length);
  }

  std::vector<Component> pool;
  pool.reserve(64);

  // bestPath, indexed by diagonal via `offset`. Stored as parallel arrays plus a
  // presence flag (jsdiff uses a sparse JS array with `undefined` holes).
  const long long offset = maxEditLength + 1;
  const long long size = 2 * maxEditLength + 3;
  std::vector<int> pathOldPos(static_cast<size_t>(size), 0);
  std::vector<int> pathLastIdx(static_cast<size_t>(size), -1);
  std::vector<char> present(static_cast<size_t>(size), 0);

  // Seed: editLength 0, common prefix on diagonal 0.
  Path seed{-1, -1};
  int newPos = extract_common(pool, seed, ap, bp, oldLen, newLen, 0);
  if (seed.oldPos + 1 >= oldLen && newPos + 1 >= newLen) {
    return build_output(pool, seed.lastIdx);
  }
  present[static_cast<size_t>(offset)] = 1;
  pathOldPos[static_cast<size_t>(offset)] = seed.oldPos;
  pathLastIdx[static_cast<size_t>(offset)] = seed.lastIdx;

  long long minDiagonalToConsider = LLONG_MIN / 4;
  long long maxDiagonalToConsider = LLONG_MAX / 4;

  const bool useTimeout = timeout_ms > 0;
  const auto startTime = std::chrono::steady_clock::now();

  for (long long editLength = 1; editLength <= maxEditLength; editLength++) {
    if (useTimeout) {
      auto elapsed = std::chrono::duration_cast<std::chrono::milliseconds>(
                       std::chrono::steady_clock::now() - startTime).count();
      if (static_cast<double>(elapsed) > timeout_ms) {
        return R_NilValue;
      }
    }

    long long lo = std::max(minDiagonalToConsider, -editLength);
    long long hi = std::min(maxDiagonalToConsider, editLength);
    for (long long diagonalPath = lo; diagonalPath <= hi; diagonalPath += 2) {
      size_t di = static_cast<size_t>(offset + diagonalPath);

      bool hasRemove = present[di - 1] != 0;
      bool hasAdd = present[di + 1] != 0;

      Path removePath{0, -1};
      if (hasRemove) {
        removePath.oldPos = pathOldPos[di - 1];
        removePath.lastIdx = pathLastIdx[di - 1];
        present[di - 1] = 0;  // no one else will read this; clear it
      }

      Path addPath{0, -1};
      bool canAdd = false;
      if (hasAdd) {
        addPath.oldPos = pathOldPos[di + 1];
        addPath.lastIdx = pathLastIdx[di + 1];
        long long addPathNewPos = static_cast<long long>(addPath.oldPos) - diagonalPath;
        canAdd = (0 <= addPathNewPos && addPathNewPos < newLen);
      }

      bool canRemove = hasRemove && (removePath.oldPos + 1 < oldLen);

      if (!canAdd && !canRemove) {
        present[di] = 0;
        continue;
      }

      Path basePath;
      if (!canRemove || (canAdd && removePath.oldPos < addPath.oldPos)) {
        basePath = add_to_path(pool, addPath, 1 /* added */, 0);
      } else {
        basePath = add_to_path(pool, removePath, 2 /* removed */, 1);
      }

      newPos = extract_common(pool, basePath, ap, bp, oldLen, newLen,
                              static_cast<int>(diagonalPath));

      if (basePath.oldPos + 1 >= oldLen && newPos + 1 >= newLen) {
        return build_output(pool, basePath.lastIdx);
      }

      present[di] = 1;
      pathOldPos[di] = basePath.oldPos;
      pathLastIdx[di] = basePath.lastIdx;
      if (basePath.oldPos + 1 >= oldLen) {
        maxDiagonalToConsider = std::min(maxDiagonalToConsider, diagonalPath - 1);
      }
      if (newPos + 1 >= newLen) {
        minDiagonalToConsider = std::max(minDiagonalToConsider, diagonalPath + 1);
      }
    }
  }

  // Exceeded maxEditLength.
  return R_NilValue;
}
