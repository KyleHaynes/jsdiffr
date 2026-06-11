// Golden outputs for patch + convert functions. Usage:
//   node gen_patch_golden.js > ../patch_golden.json
const Diff = require('diff');

function mulberry32(a) {
  return function () {
    a |= 0; a = (a + 0x6D2B79F5) | 0;
    let t = Math.imul(a ^ (a >>> 15), 1 | a);
    t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t;
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}
const rand = mulberry32(424242);
const randInt = n => Math.floor(rand() * n);
const pick = a => a[randInt(a.length)];

function randText(nLines, trailingNl) {
  const lines = [];
  const k = randInt(nLines);
  for (let i = 0; i < k; i++) lines.push(pick(['alpha', 'beta', 'gamma', 'delta', 'epsilon', '', '  indent']));
  let t = lines.join('\n');
  if (trailingNl && t.length) t += '\n';
  return t;
}

const cases = [];

// createPatch string golden + applyPatch roundtrip.
for (let i = 0; i < 500; i++) {
  const a = randText(8, rand() < 0.7);
  const b = randText(8, rand() < 0.7);
  const context = randInt(5);
  const patch = Diff.createPatch('file.txt', a, b, undefined, undefined, { context });
  const applied = Diff.applyPatch(a, patch);
  cases.push({
    type: 'createPatch', file: 'file.txt', old: a, new: b, context,
    patch, applied: applied === false ? null : applied
  });
}

// convertChangesToXML / DMP golden.
for (let i = 0; i < 200; i++) {
  const a = Array.from({ length: randInt(8) }, () => pick(['a', 'b', 'c', ' '])).join('');
  const b = Array.from({ length: randInt(8) }, () => pick(['a', 'b', 'c', ' '])).join('');
  const changes = Diff.diffChars(a, b);
  cases.push({ type: 'xml', a, b, out: Diff.convertChangesToXML(changes) });
  cases.push({
    type: 'dmp', a, b,
    out: Diff.convertChangesToDMP(changes).map(p => ({ operation: p[0], value: p[1] }))
  });
}

// reversePatch roundtrip: applying reverse of patch to b yields a (when forward applied cleanly).
for (let i = 0; i < 200; i++) {
  const a = randText(8, rand() < 0.7);
  const b = randText(8, rand() < 0.7);
  const patches = Diff.parsePatch(Diff.createPatch('f', a, b));
  const reversed = Diff.formatPatch(Diff.reversePatch(patches));
  const back = Diff.applyPatch(b, reversed);
  cases.push({ type: 'reverse', old: a, new: b, reversed, back: back === false ? null : back });
}

process.stdout.write(JSON.stringify(cases));
process.stderr.write(`generated ${cases.length} patch cases\n`);
