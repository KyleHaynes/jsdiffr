// Generates golden outputs from the real jsdiff for a large set of random and
// fixed inputs, so the R port can be checked for exact faithfulness.
// Usage: node gen_golden.js > ../golden.json

const Diff = require('diff');

// Deterministic PRNG (mulberry32) for reproducible fuzzing.
function mulberry32(a) {
  return function () {
    a |= 0; a = (a + 0x6D2B79F5) | 0;
    let t = Math.imul(a ^ (a >>> 15), 1 | a);
    t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t;
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}
const rand = mulberry32(20260611);

function randInt(n) { return Math.floor(rand() * n); }
function pick(arr) { return arr[randInt(arr.length)]; }

function randString(alphabet, maxLen) {
  const len = randInt(maxLen + 1);
  let s = '';
  for (let i = 0; i < len; i++) s += pick(alphabet);
  return s;
}

function normalize(changes) {
  return changes.map(c => ({
    value: c.value,
    added: !!c.added,
    removed: !!c.removed,
    count: c.count
  }));
}

const cases = [];

function addCase(fn, args, options) {
  let result;
  try {
    result = normalize(Diff[fn](...args, options || {}));
  } catch (e) {
    return; // skip inputs jsdiff itself rejects
  }
  cases.push({ fn, args, options: options || {}, result });
}

// ---- Fixed edge cases -----------------------------------------------------
addCase('diffChars', ['', '']);
addCase('diffChars', ['abc', 'abc']);
addCase('diffChars', ['', 'abc']);
addCase('diffChars', ['abc', '']);
addCase('diffChars', ['abc', 'abd']);
addCase('diffChars', ['ABC', 'abc'], { ignoreCase: true });
addCase('diffWords', ['The quick brown fox', 'The slow brown fox']);
addCase('diffWords', ['foo bar baz', 'foo baz']);
addCase('diffWords', ['foo bar baz', 'foo qux baz']);
addCase('diffWords', ['foo\nbar baz', 'foo baz']);
addCase('diffWords', ['foo baz', 'foo\nbar baz']);
addCase('diffWords', ['foo   bar baz', 'foo  baz']);
addCase('diffWords', ['New Value', 'New ValueMoreData'], {});
addCase('diffWordsWithSpace', ['a b  c', 'a  b c']);
addCase('diffLines', ['line\nold value\nline', 'line\nnew value\nline']);
addCase('diffLines', ['a\nb\nc\n', 'a\nB\nc\n']);
addCase('diffLines', ['a\nb\nc', 'a\nb\nc\n']);
addCase('diffTrimmedLines', ['  a\n b\n', 'a\nb\n']);
addCase('diffLines', ['a\r\nb\r\n', 'a\nb\n'], { stripTrailingCr: true });
addCase('diffSentences', ['Hello world. Foo bar.', 'Hello world. Baz bar.']);
addCase('diffCss', ['a{color:red;}', 'a{color:blue;}']);
addCase('diffArrays', [['a', 'b', 'c'], ['a', 'c', 'd']]);

// ---- Fuzzing --------------------------------------------------------------
const charAlpha = ['a', 'b', 'c', 'd'];
const wordAlpha = ['a', 'bb', 'ccc', ' ', '  ', '\t', '\n', '.', ',', '!'];
const lineAlpha = ['a', 'b', 'c', 'd', '\n', '\n', ' '];
const sentAlpha = ['a', 'b', 'c', ' ', '. ', '! ', '? ', '.', ' '];
const cssAlpha = ['a', 'b', 'color', 'red', '{', '}', ':', ';', ',', ' ', '\n'];

function fuzz(fn, alphabet, maxLen, n, options) {
  for (let i = 0; i < n; i++) {
    const a = randString(alphabet, maxLen);
    const b = randString(alphabet, maxLen);
    addCase(fn, [a, b], options);
  }
}

fuzz('diffChars', charAlpha, 12, 400);
fuzz('diffChars', charAlpha, 12, 100, { ignoreCase: true });
fuzz('diffWords', wordAlpha.concat(['A', 'BB']), 10, 600);
fuzz('diffWords', wordAlpha, 10, 100, { ignoreCase: true });
fuzz('diffWordsWithSpace', wordAlpha, 10, 400);
fuzz('diffLines', lineAlpha, 14, 400);
fuzz('diffLines', lineAlpha, 14, 100, { ignoreWhitespace: true });
fuzz('diffTrimmedLines', lineAlpha, 14, 100);
fuzz('diffSentences', sentAlpha, 12, 300);
fuzz('diffCss', cssAlpha, 16, 300);

// Array fuzzing (build arrays of tokens).
const arrAlpha = ['a', 'b', 'c', 'd', 'e'];
for (let i = 0; i < 400; i++) {
  const la = randInt(10), lb = randInt(10);
  const a = [], b = [];
  for (let j = 0; j < la; j++) a.push(pick(arrAlpha));
  for (let j = 0; j < lb; j++) b.push(pick(arrAlpha));
  addCase('diffArrays', [a, b]);
}

process.stdout.write(JSON.stringify(cases));
process.stderr.write(`generated ${cases.length} cases\n`);
