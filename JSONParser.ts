type JSONObject = { [key: string]: JSONValue };
type JSONArray = JSONValue[];
type JSONText = string;
type JSONNumber = number;
type JSONBool = boolean;
type JSONNull = null;
type JSONValue =
  | JSONObject
  | JSONArray
  | JSONText
  | JSONNumber
  | JSONBool
  | JSONNull;

function parse(text: string): JSONValue {
  const tokens = tokenize(text);
  const [obj, rest] = parseTokens(tokens);
  if (rest.length > 0) {
    throw Error(`Unexpected tokens: ${rest}`);
  }
  return obj;
}

function parseTokens(tokens: string[]): [JSONValue, string[]] {
  const [t, ...tx] = tokens;
  if (t === "{") return parseObject(tx);
  if (t.startsWith(`"`)) return [parseText(t), tx];
  if (t.match(/^[\-\.0-9]/)) return [parseFloat(t), tx];
  if (t === "true" || t === "false") return [t === "true", tx];
  if (t === "null") return [null, tx];
  throw Error(`Unexpected token ${t}, ${tx}`);
}

function parseObject(tokens: string[]): [JSONObject, string[]] {
  function go(tokens: string[], acc: JSONObject): [JSONObject, string[]] {
    const [t, ...tx] = tokens;
    if (t === "}") return [acc, tx];
    if (t === ",") return go(tx, acc);
    const [t1, ...tx1] = tx;
    if (t.startsWith(`"`) && t1 === ":") {
      const key = parseText(t);
      const [value, tx2] = parseTokens(tx1);
      return go(tx2, { ...acc, [key]: value });
    }
    throw Error(`Unexpected token 「${t}」, ${tx}`);
  }
  return go(tokens, {});
}

function splitFirstRestLast(txt: string): [string, string, string] {
  const first = txt[0];
  const rest = txt.slice(1);
  const last = rest.at(-1) ?? "";
  return [first, rest.slice(0, -1), last];
}

function parseText(textToken: string): JSONText {
  const [_first, rest, _last] = splitFirstRestLast(textToken);
  assert(_first === `"`);
  assert(_last === `"`);
  return rest;
}

function tokenize(input: string): string[] {
  const [c, cs] = [input[0], input.slice(1)];
  if (input === "") return [];
  if (c === "{") return ["{", ...tokenize(cs)];
  if (c === "}") return ["}", ...tokenize(cs)];
  if (c === ":") return [":", ...tokenize(cs)];
  if (c === " " || c === "\n") return [...tokenize(cs)];
  if (c === ",") return [",", ...tokenize(cs)];
  if (c === '"') {
    const [before, after] = splitFirstExceptEscape('"', cs);
    return [`"${before}"`, ...tokenize(after)];
  }
  if (c.match(/[\-\.0-9]/)) {
    const [before, after] = splitNotNumber(cs);
    return [c + before, ...tokenize(after)];
  }
  if (c + cs.slice(0, 3) === "true") return ["true", ...tokenize(cs.slice(3))];
  if (c + cs.slice(0, 4) === "false") return ["false", ...tokenize(cs.slice(4))];
  if (c + cs.slice(0, 3) === "null") return ["null", ...tokenize(cs.slice(3))];
  throw Error(`Unexpected token 「${c}」, ${cs}`);
}

function splitNotNumber(txt: string): [string, string] {
  const [c, cs] = [txt[0], txt.slice(1)];
  if (c.match(/[\-\.0-9]/)) {
    const [before, after] = splitNotNumber(cs);
    return [c + before, after];
  }
  return ["", txt];
}

function splitFirstExceptEscape(ch: string, txt: string): [string, string] {
  function go(txt: string) {
    if (txt === "") return ["", ""];
    const [c, cs] = [txt[0], txt.slice(1)];
    if (c === ch) return ["", cs];
    if (c === "\\") {
      const [before, after] = go(cs.slice(1));
      return [c + cs[0] + before, after];
    }
    const [before, after] = go(cs);
    return [c + before, after];
  }
  return go(txt);
}



const json = JSON.stringify({
  "foo ": "bar",
  hoge: `fu"ga`,
  a: -23.2376,
  xxxxxx: true,
  parent: {
    child: "value",
  },
  null: null,
});

const result = tokenize(json);

const expected = [
  "{",
  `"foo "`,
  ":",
  `"bar"`,
  ",",
  `"hoge"`,
  ":",
  `"fu\\"ga"`,
  ",",
  `"a"`,
  ":",
  "-23.2376",
  ",",
  `"xxxxxx"`,
  ":",
  "true",
  ",",
  `"parent"`,
  ":",
  "{",
  `"child"`,
  ":",
  `"value"`,
  "}",
  ",",
  `"null"`,
  ":",
  "null",
  "}",
];
assertEquals(result, expected);

const result2 = parse(json);
console.log(result2);

function assert(x: boolean) {
  if (!x) {
    throw Error("Fail to assertion");
  }
}

function assertEquals(left: string[], right: string[]) {
  for (let i = 0; i < left.length; i++) {
    const currentL = left[i];
    const currentR = right[i];
    if (currentL === currentR) {
      continue;
    }
    throw Error(
      `left and right are not same. index: ${i}, L: 「${currentL}」, R: 「${currentR}」`,
    );
  }
}
