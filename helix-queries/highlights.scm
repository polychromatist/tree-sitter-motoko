[
 "do"
] @keyword

[
 "debug"
 "debug_show"
] @keyword.directive

[
 "ignore"
] @keyword.control

"return" @keyword.control.return
"null" @keyword.constant
[
"if"
"else"
"switch"
"case"
] @keyword.control.conditional

[
 "and"
 "or"
 "in"
 "with"
 "not"
 "from_candid"
 "to_candid"
] @keyword.operator

[
 "label"
 "for"
 "loop"
 "while"
 "break"
 "continue"
] @keyword.control.repeat

[
 "try"
 "catch"
 "throw"
 "assert"
] @keyword.control.exception

[
 "true"
 "false"
] @constant.builtin.boolean

[
 "import"
] @keyword.control.import

[
 "private"
 "public"
 "query"
 "shared"
 "async"
 "await"
 "stable"
 "flexible"
] @keyword.storage.modifier
(exp_field "var" @keyword.storage.modifier)
(array "var" @keyword.storage.modifier)
(typ_array "var" @keyword.storage.modifier)
(typ_field "var" @keyword.storage.modifier)

[
 "let"
 "class"
 "func"
 "type"
] @keyword.storage.type
(dec_var "var" @keyword.storage.type)

[
 "actor"
 "object"
 "module"
 "system"
] @constructor

"=" @operator
[
 "!"
 "?"
 "->"
 op: (_)
 unary: (_)
 "<:"
 ":="
] @operator
(unassign) @operator
(binassign) @operator
(unop) @operator

(nat) @constant.numeric.integer
(float) @constant.numeric.float
(linecomment) @comment.line
(blockcomment) @comment.block

(typ_id top: (name) @namespace)

(typ_id name: (name) @type.builtin
 (#match? @type.builtin "^(Bool|Char|Text|Float|Int|Int8|Int16|Int32|Int64|Nat|Nat8|Nat16|Nat32|Nat64|Blob|Principal|Error)$"))

(typ_id name: (name) @type)

[
  ":"
  ";"
  ","
] @punctuation.delimiter

[
 "["
 "]"
 "{"
 "}"
 "("
 ")"
 "<"
 ">"
] @punctuation.bracket

variant_name: (name) @tag

(placeholder) @constant.builtin

(typ_tag name: (name) @tag)
(typ_field name: (name) @variable.other.member)
(typ_bind binding: (name) @constant)

(dec_func pat: (pat_plain (name) @variable.parameter))
(dec_func name: (name) @function)
(dec_type name: (name) @type)

(label name: (name) @label)
(break label: (name) @label)

(char (escape) @constant.character.escape)
(char) @constant.character

(text (escape) @constant.character.escape)
(text) @string

(call invoked: (name) @function)
(call invoked: (exp_post member: (name) @function))

(exp_post member: (name) @variable)
(exp_field name: (name) @variable.other.member)
