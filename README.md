## tree-sitter-motoko

Here you can find my attempt to reflect the [Motoko](https://github.com/dfinity/motoko) grammar into tree-sitter. Motoko is the language of the [Internet Computer](https://internetcomputer.org/), a web3 platform that serves HTTP requests through distributed, containerized applications called canisters.

The grammar used in this reflection is found [here](https://github.com/dfinity/motoko/blob/39d1b5e8a3fa8cecbd93fafad46bc22da58be584/doc/md/examples/grammar.txt).

In addition, the following non-standard syntax is supported:

 - Motoko-san syntax, e.g. `assert:invariant` and the `implies` operator
 - The `prim` keyword as used in determining a nullary type, e.g. `type Bool = prim "Bool"`

### Note about Whitespace

Motoko parses operators `" > "`, `" < "`, `" >>"` rather than `">"`, `"<"`, `">>"`. It is my understanding that the internal tree-sitter lexer takes this as a cue to, in contexts where these operators may be found, consume rather than skip the space character `0x20`. This may result in queries capturing whitespace as part of the text of a syntax node, which has results like syntax highlight queries decorating (e.g. underlining) whitespace that is ahead of the token. As far as I know, this discrepancy does not interact with the structural result of the syntax tree.
