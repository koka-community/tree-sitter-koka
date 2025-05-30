; Comment

[
  (linecomment)
  (blockcomment)
] @comment

; Literals

(string) @string
(char) @character

(escape) @string.escape

(float) @number.float
(int) @number

; Delimiters

(matchrule "|" @punctuation.delimiter)

[
  ","
  "->"
  "."
  ":"
  "::"
  "<-"
  ";"
] @punctuation.delimiter

[
  "<"
  ">"
  "("
  ")"
  "["
  "]"
  "{"
  "}"
] @punctuation.bracket

; Keywords

[
  "as"
  "behind"
  (externtarget)
  "forall"
  "handle"
  "handler"
  "in"
  "infix"
  "infixl"
  "infixr"
  ;"inject"
  "lazy"
  "mask"
  ;"other"
  (pub)
  "some"
] @keyword

[
  (con)
  ;"control"
  "ctl"
  "fn"
  "fun"
  ;"rawctl"
  ;"rcontrol"
] @keyword.function

"with" @keyword.control

[
  "elif"
  "else"
  "if"
  "match"
  "then"
] @keyword.control.conditional

[
  "import"
  ;"include"
  "module"
] @keyword.import

[
  "alias"
  "effect"
  "struct"
  "type"
  "val"
  "var"
] @keyword.storage.type

[
  "abstract"
  "co"
  "extend"
  "extern"
  "fbip"
  "final"
  "fip"
  "inline"
  "linear"
  "named"
  "noinline"
  "open"
  (override)
  "raw"
  ;"rec"
  "ref"
  "reference"
  "scoped"
  "tail"
  "value"
] @keyword.storage.modifier

"return" @keyword.control.return

; Operators

[
  "!"
  "~"
  "="
  ":="
  ; (idop)
  ; (op)
  ; (qidop)
] @operator

; Identifiers


(modulepath) @module

(typecon
  [(varid) (qvarid)] @type)

(tbinder
  (varid) @type)

(typeid
  (varid) @type)

(typedecl
  "effect"
  (varid) @type)

(paramid
  (identifier
    (varid) @variable.parameter))

(pparameter
  (pattern
    (identifier
      (varid) @variable.parameter)))

(puredecl
  (binder
    (qidentifier) @constant))
      

; Function definitions

(fundecl
  (identifier) @function)

(puredecl
  (qidentifier) @function)

; Effect definitions/usages

(opclause
  (qidentifier) @function)

(operation
  (identifier) @function)
  

; Function calls

(opexpr
  (atom
    (name) @function.call)
  .
  [
    call: "(" (arguments)? ")"
    trailing_lambda: [(block) (fnexpr)]
  ])

(opexpr
  (atom)
  (name) @function.call)

(ntlexpr
  (atom
    (name) @function.call)
  (arguments))

(ntlexpr
  (atom)
  (name) @function.call)

(argument
  [(identifier) (qimplicit)] @variable.parameter
  "="
  (expr))

[(conid) (qconid)] @constructor

[
  "initially"
  "finally"
] @function.builtin
