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

function parseTokens(tokens: Token[]): [JSONValue, Token[]] {
  const [t, ...tx] = tokens;
  if (t instanceof TokenOpenBrace) return parseObject(tx);
  if (t instanceof TokenText) return [t.value, tx];
  if (t instanceof TokenNumber) return [t.value, tx];
  if (t instanceof TokenBoolean) return [t.value, tx];
  if (t instanceof TokenNull) return [null, tx];
  throw Error(`Unexpected token ${t}, ${tx}`);
}

function parseObject(tokens: Token[]): [JSONObject, Token[]] {
  function go(tokens: Token[], acc: JSONObject): [JSONObject, Token[]] {
    const [t, ...tx] = tokens;
    if (t instanceof TokenCloseBrace) return [acc, tx];
    if (t instanceof TokenComma) return go(tx, acc);
    const [t1, ...tx1] = tx;
    if (t instanceof TokenText && t1 instanceof TokenColon) {
      const key = t.value;
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


type Token =
  | TokenOpenBrace
  | TokenCloseBrace
  | TokenColon
  | TokenComma
  | TokenText
  | TokenNumber
  | TokenBoolean
  | TokenNull;

class TokenBase {
  toString() {
    if ((this as any).value != null) {
      return `${this.constructor.name} ${(this as any).value}`;
    }
    return this.constructor.name;
  }
}
class TokenOpenBrace extends TokenBase {}
class TokenCloseBrace extends TokenBase {}
class TokenColon extends TokenBase {}
class TokenComma extends TokenBase {}
class TokenText extends TokenBase { constructor(public value: string) { super(); } }
class TokenNumber extends TokenBase { constructor(public value: number) { super(); } }
class TokenBoolean extends TokenBase { constructor(public value: boolean) { super(); } }
class TokenNull extends TokenBase {}

function tokenize(input: string): Token[] {
  const [c, cs] = [input[0], input.slice(1)];
  if (input === "") return [];
  if (c === "{") return [new TokenOpenBrace(), ...tokenize(cs)];
  if (c === "}") return [new TokenCloseBrace(), ...tokenize(cs)];
  if (c === ":") return [new TokenColon(), ...tokenize(cs)];
  if (c === " " || c === "\n") return [...tokenize(cs)];
  if (c === ",") return [new TokenComma(), ...tokenize(cs)];
  if (c === '"') {
    const [before, after] = splitFirstExceptEscape('"', cs);
    return [new TokenText(before), ...tokenize(after)];
  }
  if (c.match(/[\-\.0-9]/)) {
    const [before, after] = splitUntilNotNumber(cs);
    return [new TokenNumber(parseFloat(c + before)), ...tokenize(after)];
  }
  if (c + cs.slice(0, 3) === "true") return [new TokenBoolean(true), ...tokenize(cs.slice(3))];
  if (c + cs.slice(0, 4) === "false") return [new TokenBoolean(false), ...tokenize(cs.slice(4))];
  if (c + cs.slice(0, 3) === "null") return [new TokenNull(), ...tokenize(cs.slice(3))];
  throw Error(`Unexpected token 「${c}」, ${cs}`);
}

function splitUntilNotNumber(txt: string): [string, string] {
  const [c, cs] = [txt[0], txt.slice(1)];
  if (c.match(/[\-\.0-9]/)) {
    const [before, after] = splitUntilNotNumber(cs);
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
      return [cs[0] + before, after];
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
  new TokenOpenBrace(),
  new TokenText("foo "),
  new TokenColon(),
  new TokenText("bar"),
  new TokenComma(),
  new TokenText("hoge"),
  new TokenColon(),
  new TokenText(`fu"ga`),
  new TokenComma(),
  new TokenText("a"),
  new TokenColon(),
  new TokenNumber(-23.2376),
  new TokenComma(),
  new TokenText("xxxxxx"),
  new TokenColon(),
  new TokenBoolean(true),
  new TokenComma(),
  new TokenText("parent"),
  new TokenColon(),
  new TokenOpenBrace(),
  new TokenText("child"),
  new TokenColon(),
  new TokenText("value"),
  new TokenCloseBrace(),
  new TokenComma(),
  new TokenText("null"),
  new TokenColon(),
  new TokenNull(),
  new TokenCloseBrace(),
];
assertEquals(result, expected);

const result2 = parse(json);
console.log(result2);

function assert(x: boolean) {
  if (!x) {
    throw Error("Fail to assertion");
  }
}

function assertEquals(left: Token[], right: Token[]) {
  for (let i = 0; i < left.length; i++) {
    const currentL = left[i];
    const currentR = right[i];
    if (currentL.toString() === currentR.toString()) {
      continue;
    }
    throw Error(
      `left and right are not same. index: ${i}, L: 「${currentL}」, R: 「${currentR}」`,
    );
  }
}
