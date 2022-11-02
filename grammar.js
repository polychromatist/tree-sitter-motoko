const PREC = {
  PAT_VARIANT: 103,
  PAT_VARIANT_NULLARY: 102,
  PAT_OPTION: 101,
  
  IF: 0,
  LOOP: 0,
  ELSE: 1,
  WHILE: 1,
  ASSIGN: 2,
  COLON: 3,
  OR: 4,
  AND: 5,
  RELOP: 6,
  ADDSUB: 7,
  CONCAT: 7,
  ADDSUBWRAP: 7,
  MULDIV: 8,
  MOD: 8,
  MULWRAP: 8,
  BOR: 9,
  BAND: 10,
  BXOR: 11,
  BSHIFT: 12,
  BROTATE: 12,
  EXP: 13,
  EXPWRAP: 13,
  INDEX: 20,
  CALL: 20,
  
  TYPEASSIGN: 29,
  TYPEAND: 30,
  TYPEOR: 31,
};

const _list = (token, sep) => optional(seq(token, repeat(seq(sep, token)), optional(sep)));
const _list1 = (token, sep) => seq(token, repeat(seq(sep, token)), optional(sep));
const _list_s = (token, sep) => repeat(seq(token, sep));
const _binop = (token, op) => seq(field("arg0", token), field("op", op), field("arg1", token));
const _annotation = (name, token) => seq(":", field(name + "_type", token));
const _fish = (token) => seq("<", _list(token, ","), ">");

const WHITESPACE = /\s/;
const NAME = /[a-zA-Z_][0-9a-zA-Z_]*/;
const DECIMAL_DIGIT = /[0-9]/;
const HEX_DIGIT = /[0-9a-f-A-F]/;

const ASCII = /[\x00-\x7f]/u;
const ASCII_NO_NL = /[\x00-\x09\x0b-\x7f]/u;

const ESCAPE = /[nrt\\'"]/

const DECIMAL_NUM = /[0-9][0-9_]*/
const HEX_NUM = /0x[0-9a-fA-F][0-9a-fA-F_]*/

const FLOAT = [
  /[0-9][0-9_]*\.(?:[0-9][0-9_]*)?/,
  /[0-9][0-9_]*(?:\.(?:[0-9][0-9_]*)?)?(?:e|E)(?:\+|-)?[0-9][0-9_]*/,
  /0x[0-9a-fA-F][0-9a-fA-F_]*\.(?:[0-9a-fA-F][0-9a-fA-F_]*)/,
  /0x[0-9a-fA-F][0-9a-fA-F_]*(?:\.(?:[0-9a-fA-F][0-9a-fA-F_]*)?)?(?:p|P)(?:\+|-)?[0-9a-fA-F][0-9a-fA-F_]*/
];

const UTF8_ENC = [
  /[\xc2-\xdf][\x80-\xbf]/, 
  /[\xe0][\xa0-\xbf][\x80-\xbf]/,
  /[\xed][\x80-\x9f][\x80-\xbf]/,
  /[\xe1-\xec\xee-\xef][\x80-\xbf]{2}/,
  /[\xf0][\x90-\xbf][\x80-\xbf]{2}/,
  /[\xf4][\x80-\x8f][\x80-\xbf]{2}/,
  /[\xf1-\xf3][\x80-\xbf]{3}/
];

module.exports = grammar({
  name: 'motoko',
  
  supertypes: $ => [$.dec_nonvar, $.exp_nondec, $.lit, $.exp_nullary, $.typ_nullary, $.pat_plain],
  conflicts: $ => [
    [$.exp_field, $.exp_nullary],
    [$._exp_obj_compose, $._exp_un_raw],
    [$.exp_field, $.dec],
    [$._dec_var_typ, $._exp_field_typ],
    [$._typ_func_short_fish, $._typ_func_fish],
    [$.dec_var, $.exp_field],
  ],
  word: $ => $.name,
  extras: $ => [WHITESPACE, $.linecomment, $.blockcomment],
  inline: $ => [$.character, $.exp_nonvar, $.pat_nullary, $.exp_nest],
  externals: $ => [$._text_literal, $.blockcomment],
  
  rules: {
    prog: $ => seq(
      _list_s($.imp, ";"),
      _list_s($.dec, ";")),
    
    imp: $ => seq(
      "import",
      $.pat_nullary,
      optional("="),
      $.text),
    
    dec_field: $ => seq(
      optional(field("vis", $.vis)),
      optional(field("stab", $.stab)),
      $.dec),
    
    dec: $ => choice(
      $.dec_var,
      $.dec_nonvar,
      $.exp_nondec),
    
    block: $ => seq("{", _list($.dec, ";"), "}"),
    
    dec_var: $ => seq(
      "var",
      field("name", $.name),
      optional($._dec_var_typ),
      "=",
      field("value", $.exp)),
    _dec_var_typ: $ => _annotation("var", $.typ),
    
    dec_nonvar: $ => choice(
      $.dec_let,
      $.dec_type,
      $.dec_obj,
      $.dec_func,
      $.dec_class),
    
    dec_let: $ => seq(
      "let",
      field("pat", $.pat),
      "=",
      field("value", $.exp)),
    
    dec_type: $ => seq(
      "type",
      field("name", $.name),
      optional($._dec_type_fish),
      "=",
      field("type", $.typ)),
    _dec_type_fish: $ => _fish($.typ_bind),
    
    dec_obj: $ => seq(
      field("sort", $.obj_sort),
      optional(field("name", $.name)),
      $._obj_body),
    _obj_body: $ => seq("{", _list($.dec_field, ";"), "}"),
    
    dec_func: $ => seq(
      optional(field("opt", $.shared_pat_opt)),
      "func",
      optional(field("name", $.name)),
      optional($._dec_func_fish),
      field("pat", $.pat_plain),
      optional($._dec_func_typ_return),
      $._func_body),
    _func_body: $ => choice(
      seq("=", field("body", $.exp)),
      field("body", $.block)),
    _dec_func_typ_return: $ => _annotation("return", $.typ),
    _dec_func_fish: $ => _fish($.typ_bind),
    
    dec_class: $ => seq(
      optional(field("opt", $.shared_pat_opt)),
      optional(field("sort", $.obj_sort)),
      "class",
      optional(field("name", $.name)),
      optional($._dec_class_fish),
      field("pat", $.pat_plain),
      optional($._dec_class_typ),
      $._class_body),
    _class_body: $ => choice(
      seq("=", optional(field("obj_name", $.name)), $._obj_body),
      $._obj_body),
    _dec_class_typ: $ => _annotation("class", $.typ),
    _dec_class_fish: $ => _fish($.typ_bind),
    
    exp_obj: $ => seq("{", choice($._exp_obj_fields, $._exp_obj_compose), "}"),
    _exp_obj_fields: $ => _list1($.exp_field, ";"),
    _exp_obj_compose: $ => seq($._exp_post_raw, choice(
      repeat1(seq("and", $._exp_post_raw)),
      seq(repeat(seq("and", $._exp_post_raw)), $._exp_obj_with))),
    _exp_obj_with: $ => seq("with", _list1($.exp_field, ";")),
    
    exp_field: $ => prec.dynamic(2, seq(
      optional("var"),
      field("name", $.name),
      optional($._exp_field_typ),
      optional(seq("=", field("value", $.exp))))),
    _exp_field_typ: $ => prec.dynamic(2, _annotation("field", $.typ)),
    
    exp_plain: $ => choice(
      $.lit,
      seq("(", _list($.exp, ","), ")")),
    
    exp_nullary: $ => choice(
      $.exp_obj,
      $.exp_plain,
      $.name),
    
    _exp_post_raw: $ => choice(
      $.exp_nullary,
      $.array,
      $.call,
      $.sec_ctor,
      $.exp_post
      ),
    
    exp_post: $ => choice(
      $._exp_index,
      $._exp_member,
      $._exp_bang),
    array: $ => seq("[",
      optional("var"),
      _list($.exp_nonvar, ","),
      "]"),
    _exp_index: $ => prec.left(PREC.INDEX, seq($._exp_post_raw, "[", field("index", $.exp), "]")),
    _exp_member: $ => seq($._exp_post_raw, ".", field("member", choice($.nat, $.name))),
    _exp_bang: $ => prec.left(seq($._exp_post_raw, field("unary", "!"))),

    call: $ => prec.left(PREC.CALL, seq(
      field("invoked", $._exp_post_raw),
      optional($._call_fish),
      field("arg", $.exp_nullary)
    )),
    _call_fish: $ => seq("<", _list($.typ, ","), ">"),
    
    sec_ctor: $ => seq(
      "(", "system", field("library", $._exp_post_raw),
      ".", field("name", $.name), ")"),   
    unop: () => choice("-", "+", "^"),
    unassign: () => choice("+=", "-=", "^="),
    
    _exp_un_raw: $ => prec.dynamic(1, choice(
      $._exp_post_raw,
      $.exp_variant,
      $.exp_un,
      )),
    
    exp_un: $ => prec.dynamic(1, choice(
      $._exp_option,
      $._exp_unassign,
      $._exp_unop,
      $._actor,
      $._not,
      $._debug_show,
      $._to_candid,
      $._from_candid)),
    exp_variant: $ => seq(
      "#",
      field("variant", $.name),
      optional(field("value", $.exp_nullary))),
    _exp_option: $ => prec.right(seq(field("unary", "?"), $._exp_un_raw)),
    _exp_unop: $ => prec.right(seq($.unop, $._exp_un_raw)),
    _exp_unassign: $ => prec.right(seq($.unassign, $._exp_un_raw)),
    
    _actor: $ => seq(field("unary", "actor"), $.exp_plain),
    
    _not: $ => prec.right(seq(field("unary", "not"), $._exp_un_raw)),
    
    _debug_show: $ => prec.right(seq(field("unary", "debug_show"), $._exp_un_raw)),
    
    _to_candid: $ => seq(field("unary", "to_candid"), "(", _list($.exp, ","), ")"),
    
    _from_candid: $ => prec.right(seq(field("unary", "from_candid"), $._exp_un_raw)),
    
    _exp_bin_raw: $ => choice(
      $.exp_bin,
      $._exp_un_raw,
      $.exp_annotated),
    
    exp_bin: $ => choice(
      ...[
        ["or", PREC.OR],
        ["and", PREC.AND],
        ["+", PREC.ADDSUB],
        ["-", PREC.ADDSUB],
        ["*", PREC.MULDIV],
        ["/", PREC.MULDIV],
        ["%", PREC.MOD],
        ["+%", PREC.ADDSUBWRAP],
        ["-%", PREC.ADDSUBWRAP],
        ["*%", PREC.MULWRAP],
        ["|", PREC.BOR],
        ["&", PREC.BAND],
        ["^", PREC.BXOR],
        ["#", PREC.CONCAT],
        ["**", PREC.EXP],
        ["**%", PREC.EXPWRAP]
        ].map(([s, p]) => prec.left(p, _binop($._exp_bin_raw, s) )),
      // technically there's no precedence, but we need to resolve the conflict for the parser
      // choose left
      ...[
        ["==", PREC.RELOP],
        ["!=", PREC.RELOP],
        [" > ", PREC.RELOP],
        [" < ", PREC.RELOP],
        ["<=", PREC.RELOP],
        [">=", PREC.RELOP],
        ["<<", PREC.BSHIFT],
        [" >>", PREC.BSHIFT],
        ["<<>", PREC.BROTATE],
        ["<>>", PREC.BROTATE],
        ].map(([s, p]) =>
          prec(p, prec.left(p, _binop($._exp_bin_raw, s)) )),
    ),
    exp_annotated: $ => prec.left(PREC.COLON, seq($._exp_bin_raw, ":", field("annotation", $._typ_nobin_raw))),
    
    exp_nondec: $ => choice(
      $.exp_bin,
      $.exp_un,
      $.exp_nullary,
      $.array,
      $.call,
      $.sec_ctor,
      $.exp_post,
      $.exp_variant,
      $.exp_annotated,
      $.assign,
      $.binassign,
      $.return,
      $.async,
      $.await,
      $.assert,
      $.label,
      $.break,
      $.continue,
      $.debug,
      $.if,
      $.ifelse,
      $.try,
      $.throw,
      $.switch,
      $.while,
      $.loop,
      $.loopwhile,
      $.for,
      $.ignore,
      $.do,
      $.do_option
    ),
    
    assign: $ => prec.right(PREC.ASSIGN, seq(
      field("binding", $._exp_bin_raw),
      field("op", ":="),
      field("value", $.exp))),
    
    binassign: $ => choice(
      ...[
        "+=",
        "-=",
        "*=",
        "/=",
        "%=",
        "**-",
        "+%=",
        "-%=",
        "*%=",
        "**%=",
        "&=",
        "|=",
        "^=",
        "<<=",
        ">>=",
        "<<>=",
        "<>>=",
        "@="
        ].map(s => prec.right(PREC.ASSIGN, seq(
          field("binding", $._exp_bin_raw),
          field("op", s),
          field("value", $.exp))))),
    
    return: $ => prec.right(seq("return", optional(field("with", $.exp)))),
    
    async: $ => seq("async", $.exp_nest),
    
    await: $ => seq("await", $.exp_nest),
    
    assert: $ => seq("assert", $.exp_nest),
    
    label: $ => seq(
      "label",
      field("name", $.name),
      optional($._label_typ),
      $.exp_nest),
    _label_typ: $ => _annotation("label", $.typ),
    
    break: $ => seq(
      "break",
      field("label", $.name),
      optional(field("with", $.exp_nullary))),
    
    continue: $ => seq("continue", field("label", $.name)),
    
    debug: $ => seq("debug", $.exp_nest),
    
    if: $ => prec(PREC.IF, seq(
      "if",
      field("antecedent", $.exp_nullary),
      field("consequence", $.exp_nest))),
    
    ifelse: $ => prec(PREC.ELSE, seq(
      "if",
      field("antecedent", $.exp_nullary),
      field("consequence", $.exp_nest),
      "else",
      field("fallback", $.exp_nest))),
    
    try: $ => seq(
      "try",
      field("body", $.exp_nest),
      $.catch),
    
    catch: $ => seq(
      "catch",
      field("guard", $.pat_nullary),
      field("handler", $.exp_nest)),
    
    throw: $ => seq("throw", $.exp_nest),
    
    switch: $ => seq(
      "switch",
      field("control", $.exp_nullary),
      "{", _list_s($.case, ";"), "}"),
    
    case: $ => seq(
      "case",
      field("candidate", $.pat_nullary),
      field("consequence", $.exp_nest)),
    
    while: $ => seq(
      "while",
      field("control", $.exp_nullary),
      field("body", $.exp_nest)),
    
    loop: $ => prec(PREC.LOOP, seq("loop", $.exp_nest)),
    
    loopwhile: $ => prec(PREC.WHILE, seq(
      "loop",
      field("loop_body", $.exp_nest),
      "while",
      field("while_body", $.exp_nest))),
    
    for: $ => seq(
      "for",
      "(", field("binding", $.pat), "in", field("iterator", $.exp), ")",
      field("body", $.exp_nest)),
    
    ignore: $ => seq("ignore", $.exp_nest),
    
    do: $ => seq("do", $.block),
    
    do_option: $ => seq("do", "?", $.block),
    
    exp_nonvar: $ => choice($.exp_nondec, $.dec_nonvar),
    
    exp: $ => choice($.exp_nonvar, $.dec_var),
    
    exp_nest: $ => choice($.block, $.exp),
    
    pat_field: $ => seq(
      field("field", $.name),
      optional(seq(":", field("field_type", $.typ))),
      optional(seq("=", field("value", $.pat)))),
    
    pat: $ => $._pat_bin_raw,
    
    _pat_bin_raw: $ => choice(
      $._pat_un_raw,
      $.pat_bin),
    
    pat_bin: $ => choice(
      $._pat_or,
      $._pat_annotated),
    _pat_or: $ => prec.left(1, _binop($._pat_bin_raw, "or")),
    _pat_annotated: $ => seq($._pat_bin_raw, ":", field("pat_type", $.typ)),
    
    _pat_un_raw: $ => choice(
      $.pat_nullary,
      $.pat_un),
    
    pat_un: $ => choice(
      $._pat_variant,
      $._pat_variant_nullary,
      $._pat_option,
      seq($.unop, $.lit)),
    _pat_variant: $ => seq(
      field("unary", "#"),
      field("variant_name", $.name)),
    _pat_variant_nullary: $ => prec.right(PREC.PAT_VARIANT_NULLARY, seq(
      field("unary", "#"),
      field("variant_name", $.name),
      $.pat_nullary)),
    _pat_option: $ => prec.right(PREC.PAT_OPTION, seq(
      field("unary", "?"),
      field("option", $._pat_un_raw))),
    
    pat_nullary: $ => choice(
      $.pat_plain,
      seq("{", _list($.pat_field, ";"), "}")),
    
    pat_plain: $ => choice(
      $.placeholder,
      $.name,
      $.lit,
      $.pat_tuple),
    pat_tuple: $ => seq("(", _list($._pat_bin_raw, ","), ")"),
      
    shared_pat_opt: $ => choice(
      seq("shared", optional("query"), optional($.pat_plain)),
      seq("query", optional($.pat_plain))
    ),
    
    typ_obj: $ => seq("{", _list($.typ_field, ";"), "}"),
    
    typ_field: $ => choice(
      $._typ_component,
      $._typ_mut_immut,
      $._typ_func_short),
    _typ_component: $ => seq(
      "type",
      field("name", $.name),
      optional($._typ_component_fish),
      "=", field("value", $.typ)),
    _typ_component_fish: $ => _fish($.typ_bind),
    _typ_mut_immut: $ => seq(optional("var"), field("name", $.name), ":", $.typ),
    _typ_func_short: $ => seq(
      field("name", $.name),
      optional($._typ_func_short_fish),
      field("param", $.typ),
      ":",
      field("return", $.typ)),
    _typ_func_short_fish: $ => _fish($.typ_bind),
    
    typ_variant: $ => choice(
      seq("{", "#", "}"),
      seq("{", _list1($.typ_tag, ";"), "}")),
    
    typ_nullary: $ => choice(
      $.typ_tuple,
      $.typ_id,
      $.typ_array,
      $.typ_obj,
      $.typ_variant),
    
    typ_tuple: $ => seq("(", _list($.typ_item, ","), ")"),
    
    typ_id: $ => seq(choice(field("name", $.name), $._typ_id_member), optional($.typ_args)),
    _typ_id_member: $ => seq(
      field("top", $.name),
      repeat(seq(".", $.name)),
      ".", field("name", $.name)),
    
    typ_array: $ => seq("[", optional("var"), $.typ, "]"),
    
    _typ_un_raw: $ => choice(
      $.typ_nullary,
      $.typ_un),
    
    typ_un: $ => seq("?", $._typ_un_raw),
    
    _typ_pre_raw: $ => choice(
      $._typ_un_raw,
      $.typ_pre
      ),
    
    typ_pre: $ => choice(
      seq("async", $._typ_pre_raw),
      seq($.obj_sort, $.typ_obj)),
    
    _typ_nobin_raw: $ => choice(
      $._typ_pre_raw,
      $.typ_func
      ),
    
    //typ_nobin: $ => $.typ_func,
    
    typ_func: $ => seq(
      optional($.func_sort_opt),
      optional($._typ_func_fish),
      field("param", $._typ_un_raw),
      "->",
      field("return", $._typ_nobin_raw)),
    _typ_func_fish: $ => prec.dynamic(1, _fish($.typ_bind)),
    
    func_sort_opt: () => choice(
      seq("shared", optional("query")),
      "query"),
    
    typ: $ => choice(
      $._typ_nobin_raw,
      $.typ_and,
      $.typ_or),
    
    typ_and: $ => prec.left(PREC.TYPEAND, seq(_binop($.typ, "and"))),
    typ_or: $ => prec.left(PREC.TYPEOR, seq(_binop($.typ, "or"))),
    
    typ_item: $ => choice(
      seq(field("name", $.name), ":", field("annotation", $.typ)),
      $.typ),
    
    typ_args: $ => _fish($.typ),
    
    typ_tag: $ => seq("#", field("name", $.name), optional(seq(":", $.typ))),
    
    typ_bind: $ => choice(
      seq(field("binding", $.name), "<:", $.typ),
      field("binding", $.name)),
    
    lit: $ => choice(
      "null",
      $.bool,
      $.nat,
      $.float,
      $.char,
      $.text),
    
    bool: () => choice("true", "false"),
    
    vis: () => choice("private", "public", "system"),
    stab: () => choice("flexible", "stable"),
    
    obj_sort: () => choice("object", "actor", "module"),
    
    placeholder: () => "_",
    
    name: () => NAME,
    
    linecomment: () => token(seq("//", /.*/)),
    
    nat: () => choice(DECIMAL_NUM, HEX_NUM),
    float: () => choice(...FLOAT),
    
    utf8_enc: () => choice(...UTF8_ENC),
    
    _utf8: $ => choice(ASCII, $.utf8_enc),
    _utf8_no_nl: $ => choice(ASCII_NO_NL, $.utf8_enc),
    
    escape: () => seq("\\", token.immediate(ESCAPE)),
    byte: () => seq("\\", token.immediate(HEX_DIGIT), token.immediate(HEX_DIGIT)),
    utf8code: () => seq("\\u{", token.immediate(HEX_NUM), token.immediate("}")),
    
    character: $ => choice(
      /[^"\\\x00-\x1f\x7f-\xff]/,
      $.utf8_enc,
      $.escape,
      $.byte,
      $.utf8code),
    
    char: $ => seq("'", $.character, token.immediate("'")),
    
    text: $ => seq('"', repeat(choice($.escape, $._text_literal)), token.immediate('"'))
  }
})
