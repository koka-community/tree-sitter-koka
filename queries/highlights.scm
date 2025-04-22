; Comment

[
  (linecomment)
  (blockcomment)
] @comment

; Literals

[
  (string)
  (char)
] @string

(escape) @constant.character.escape

(float) @constant.numeric.float
(int) @constant.numeric.integer

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
] @keyword.control.import

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
  (idop)
  (op)
  (qidop)
] @operator

; Identifiers

[(conid) (qconid)] @constructor

[(varid) (qvarid)] @variable

(modulepath (varid) @namespace)

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

; (operation
;   (identifier
;     [(varid) (idop)] @function))

; (fundecl
;   (identifier) @function)

(puredecl
  (qidentifier) @function)
  

; Function calls

[
  "initially"
  "finally"
] @function.special
