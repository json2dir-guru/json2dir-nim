# json2dir: create the directory tree a JSON document describes, in the current directory.
# Implements RFC J2D-1 (https://github.com/kitsunoff/awesome-json2dir/blob/main/spec/rfc-json2dir.md).
# Nim standard library only; the JSON parser is hand-written (std/json is too lax).

import std/[os, posix, tables, algorithm]

type
  Json2dirError = object of CatchableError
  Kind = enum kString, kArray, kObject
  Node = ref object
    case kind: Kind
    of kString: str: string
    of kArray: items: seq[Node]
    of kObject:
      names: seq[string]
      values: Table[string, Node]

proc fail(msg: string) {.noreturn.} =
  raise newException(Json2dirError, msg)

# ---- §3: strict UTF-8 ----

proc checkUtf8(s: string) =
  var i = 0
  while i < s.len:
    let c = ord(s[i])
    var n, cp, lo: int
    if c < 0x80: inc i; continue
    elif c >= 0xC2 and c <= 0xDF: n = 1; cp = c and 0x1F; lo = 0x80
    elif c >= 0xE0 and c <= 0xEF: n = 2; cp = c and 0x0F; lo = 0x800
    elif c >= 0xF0 and c <= 0xF4: n = 3; cp = c and 0x07; lo = 0x10000
    else: fail("input is not valid UTF-8")
    if i + n > s.len - 1: fail("input is not valid UTF-8")
    for k in 1..n:
      let b = ord(s[i + k])
      if (b and 0xC0) != 0x80: fail("input is not valid UTF-8")
      cp = (cp shl 6) or (b and 0x3F)
    if cp < lo or cp > 0x10FFFF or (cp >= 0xD800 and cp <= 0xDFFF):
      fail("input is not valid UTF-8")
    i += n + 1

# ---- §3: strict JSON ----

type Parser = object
  s: string
  i: int

proc ws(p: var Parser) =
  while p.i < p.s.len and p.s[p.i] in {' ', '\t', '\n', '\r'}: inc p.i

proc expect(p: var Parser, c: char) =
  p.ws
  if p.i >= p.s.len or p.s[p.i] != c: fail("input is not valid JSON: expected '" & c & "' at byte " & $p.i)
  inc p.i

proc addUtf8(r: var string, cp: int) =
  if cp < 0x80: r.add chr(cp)
  elif cp < 0x800:
    r.add chr(0xC0 or (cp shr 6)); r.add chr(0x80 or (cp and 0x3F))
  elif cp < 0x10000:
    r.add chr(0xE0 or (cp shr 12)); r.add chr(0x80 or ((cp shr 6) and 0x3F)); r.add chr(0x80 or (cp and 0x3F))
  else:
    r.add chr(0xF0 or (cp shr 18)); r.add chr(0x80 or ((cp shr 12) and 0x3F))
    r.add chr(0x80 or ((cp shr 6) and 0x3F)); r.add chr(0x80 or (cp and 0x3F))

proc hex4(p: var Parser): int =
  if p.i + 4 > p.s.len: fail("input is not valid JSON: bad \\u escape")
  for k in 0..3:
    let c = p.s[p.i + k]
    let d = case c
      of '0'..'9': ord(c) - ord('0')
      of 'a'..'f': ord(c) - ord('a') + 10
      of 'A'..'F': ord(c) - ord('A') + 10
      else: fail("input is not valid JSON: bad \\u escape")
    result = result * 16 + d
  p.i += 4

proc parseString(p: var Parser): string =
  p.expect('"')
  while true:
    if p.i >= p.s.len: fail("input is not valid JSON: unterminated string")
    let c = p.s[p.i]
    inc p.i
    if c == '"': return
    if ord(c) < 0x20: fail("input is not valid JSON: control character in string")
    if c != '\\': result.add c; continue
    if p.i >= p.s.len: fail("input is not valid JSON: unterminated string")
    let e = p.s[p.i]
    inc p.i
    case e
    of '"': result.add '"'
    of '\\': result.add '\\'
    of '/': result.add '/'
    of 'b': result.add '\b'
    of 'f': result.add '\f'
    of 'n': result.add '\n'
    of 'r': result.add '\r'
    of 't': result.add '\t'
    of 'u':
      var cp = p.hex4
      if cp >= 0xDC00 and cp <= 0xDFFF: fail("string contains an unpaired surrogate")
      if cp >= 0xD800 and cp <= 0xDBFF:
        if p.i + 2 > p.s.len or p.s[p.i] != '\\' or p.s[p.i + 1] != 'u':
          fail("string contains an unpaired surrogate")
        p.i += 2
        let lo = p.hex4
        if lo < 0xDC00 or lo > 0xDFFF: fail("string contains an unpaired surrogate")
        cp = 0x10000 + ((cp - 0xD800) shl 10) + (lo - 0xDC00)
      result.addUtf8 cp
    else: fail("input is not valid JSON: bad escape")

proc parseValue(p: var Parser, depth: int): Node =
  if depth > 1000: fail("document is nested too deeply")
  p.ws
  if p.i >= p.s.len: fail("input is not valid JSON: unexpected end of input")
  case p.s[p.i]
  of '"': result = Node(kind: kString, str: p.parseString)
  of '[':
    inc p.i
    result = Node(kind: kArray)
    p.ws
    if p.i < p.s.len and p.s[p.i] == ']': inc p.i; return
    while true:
      result.items.add p.parseValue(depth + 1)
      p.ws
      if p.i < p.s.len and p.s[p.i] == ',': inc p.i; continue
      p.expect(']'); return
  of '{':
    inc p.i
    result = Node(kind: kObject)
    p.ws
    if p.i < p.s.len and p.s[p.i] == '}': inc p.i; return
    while true:
      p.ws
      let name = p.parseString
      p.expect(':')
      let v = p.parseValue(depth + 1)
      if name notin result.values: result.names.add name
      result.values[name] = v   # duplicates: the last occurrence wins
      p.ws
      if p.i < p.s.len and p.s[p.i] == ',': inc p.i; continue
      p.expect('}'); return
  else:
    # numbers, true, false, null have no meaning (§4.5); anything else is not JSON
    fail("input is not valid JSON, or contains a number, boolean or null at byte " & $p.i)

proc parse(s: string): Node =
  var src = s
  if src.len >= 3 and src[0..2] == "\xEF\xBB\xBF": src = src[3..^1]
  checkUtf8(src)
  var p = Parser(s: src, i: 0)
  result = p.parseValue(0)
  p.ws
  if p.i != p.s.len: fail("input is not valid JSON: trailing data")

# ---- §4: validation ----

proc validate(n: Node, where: string) =
  case n.kind
  of kString: discard
  of kArray:
    if n.items.len != 2 or n.items[0].kind != kString or n.items[1].kind != kString:
      fail(where & ": an array must be [\"link\", target] or [\"script\", content]")
    let k = n.items[0].str
    if k != "link" and k != "script": fail(where & ": unknown array kind " & k)
    if k == "link" and '\0' in n.items[1].str: fail(where & ": a link target cannot contain NUL")
  of kObject:
    for name in n.names:
      let child = if where == ".": name else: where & "/" & name
      if name == "" or name == "." or name == ".." or '/' in name or '\0' in name:
        fail(child & ": invalid name")
      validate(n.values[name], child)

# ---- §5: applying to the file system (never following links) ----

proc osFail(path: string) {.noreturn.} =
  fail(path & ": " & $strerror(errno))

proc lstatKind(path: string, isDir: var bool): bool =
  ## true if something exists at path
  var st: Stat
  if lstat(path.cstring, st) != 0:
    if errno == ENOENT: return false
    osFail(path)
  isDir = S_ISDIR(st.st_mode)
  true

proc clear(path: string) =
  var isDir: bool
  if lstatKind(path, isDir):
    if isDir: fail(path & ": a directory is in the way")
    if unlink(path.cstring) != 0: osFail(path)

var O_NOFOLLOW {.importc, header: "<fcntl.h>".}: cint

proc writeFile(path, content: string, executable: bool) =
  let fd = posix.open(path.cstring, O_WRONLY or O_CREAT or O_EXCL or O_NOFOLLOW, 0o666)
  if fd < 0: osFail(path)
  var off = 0
  while off < content.len:
    let w = posix.write(fd, unsafeAddr content[off], content.len - off)
    if w < 0:
      if errno == EINTR: continue
      discard posix.close(fd); osFail(path)
    off += w
  if executable:
    var st: Stat
    if fstat(fd, st) != 0 or fchmod(fd, Mode((st.st_mode.int and 0o7777) or 0o111)) != 0:
      discard posix.close(fd); osFail(path)
  if posix.close(fd) != 0: osFail(path)

proc apply(dir: string, tree: Node) =
  var names = tree.names
  names.sort(system.cmp[string])   # ascending UTF-8 byte order
  for name in names:
    let path = dir & "/" & name
    let v = tree.values[name]
    case v.kind
    of kString:
      clear(path); writeFile(path, v.str, false)
    of kArray:
      clear(path)
      if v.items[0].str == "link":
        if symlink(v.items[1].str.cstring, path.cstring) != 0: osFail(path)
      else:
        writeFile(path, v.items[1].str, true)
    of kObject:
      var isDir: bool
      let exists = lstatKind(path, isDir)
      if not exists or not isDir:
        if exists and unlink(path.cstring) != 0: osFail(path)
        if mkdir(path.cstring, 0o777) != 0: osFail(path)
      apply(path, v)

proc main(): int =
  if paramCount() > 0:
    stderr.write "usage: json2dir < document.json\n"
    return 2
  try:
    let doc = parse(readAll(stdin))
    if doc.kind != kObject: fail("the root of the document must be an object")
    validate(doc, ".")
    apply(".", doc)
    return 0
  except Json2dirError, IOError, OSError:
    stderr.write "json2dir: " & getCurrentExceptionMsg() & "\n"
    return 1

quit(main())
