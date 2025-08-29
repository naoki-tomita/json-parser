type JSONObject = { [key: string]: JSONValue }
type JSONArray = JSONValue[];
type JSONText = string;
type JSONNumber = number;
type JSONBool = boolean;
type JSONNull = null;
type JSONValue
  = JSONObject
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
  if (t === "{") {
    return parseObject(tokens)
  }
  if (t.startsWith(`"`)) {
    return [parseText(t), tx];
  }
  if (t.match(/^[\-\.0-9]/)) {
    return [parseFloat(t), tx];
  }
  if (t === "true" || t === "false") {
    return [t === "true", tx];
  }
  if (t === "null") {
    return [null, tx];
  }
  throw Error(`Unexpected token ${t}`);
}

function parseObject(_tokens: string[]): [JSONObject, string[]] {
  let [_braceOpen, ...tokens] = _tokens;
  assert(_braceOpen === "{");
  let result = {};
  while (tokens[0] !== "}" || tokens.length > 0) {
    const [rawKey, _colon, ...others] = tokens;
    assert(_colon === ":");
    const key = parseText(rawKey);
    const [value, vx] = parseTokens(others);
    const [commaOrCloseBrace, ...tx] = vx;
    result = { ...result, [key]: value };
    if (commaOrCloseBrace === "}") {
      return [result, tx.slice(0)];
    }
    tokens = tx;
  }
  throw Error("Unexpected end of token");
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
  if (input === "") {
    return [];
  } else if (c === "{") {
    return ["{", ...tokenize(cs)];
  } else if (c === "}") {
    return ["}", ...tokenize(cs)];
  } else if (c === ":") {
    return [":", ...tokenize(cs)];
  } else if (c === " " || c === "\n") {
    return [...tokenize(cs)];
  } else if (c === ",") {
    return [",", ...tokenize(cs)];
  } else if (c === '"') {
    const [before, after] = splitFirstExceptEscape('"', cs);
    return [`"${before}"`, ...tokenize(after)];
  } else if (c.match(/[\-\.0-9]/)) {
    const [before, after] = splitNotNumber(cs);
    return [c + before, ...tokenize(after)];
  } else if (c + cs.slice(0, 3) === "true") {
    return ["true", ...tokenize(cs.slice(3))];
  } else if (c + cs.slice(0, 4) === "false") {
    return ["false", ...tokenize(cs.slice(4))];
  } else if (c + cs.slice(0, 3) === "null") {
    return ["null", ...tokenize(cs.slice(3))];
  }
  throw Error(`Unexpected token ${c}`);
}

function splitNotNumber(txt: string): [string, string] {
  const [c, cs] = [txt[0], txt.slice(1)];
  if (c.match(/[\-\.0-9]/)) {
    const [before, after] = splitNotNumber(cs);
    return [c + before, after];
  } else {
    return ["", txt];
  }
}

function splitFirstExceptEscape(ch: string, txt: string): [string, string] {
  function go(txt: string) {
    if (txt === "") {
      return ["", ""];
    }
    const [c, cs] = [txt[0], txt.slice(1)];
    switch (c) {
      case ch: {
        return ["", cs];
      }
      case "\\": {
        const [before, after] = go(cs.slice(1));
        return [c + cs[0] + before, after];
      }
      default: {
        const [before, after] = go(cs);
        return [c + before, after];
      }
    }
  }
  return go(txt);
}
const json = JSON.stringify({
  "foo ": "bar",
  hoge: `fu"ga`,
  a: -23.2376,
  xxxxxx: true,
  parent: {
    child: "value"
  },
  null: null,
});

const result = tokenize(json);

const expected = [
  "{",
  `"foo "`, ":", `"bar"`, ",",
  `"hoge"`, ":", `"fu\\"ga"`, ",",
  `"a"`, ":", "-23.2376", ",",
  `"xxxxxx"`, ":", "true", ",",
  `"parent"`, ":", "{", `"child"`, ":", `"value"`, "}", ",",
  `"null"`, ":", "null",
  "}"];
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
    throw Error(`left and right are not same. index: ${i}, L: 「${currentL}」, R: 「${currentR}」`);
  }
}
