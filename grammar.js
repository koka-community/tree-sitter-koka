/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

module.exports = grammar({
  name: "koka",
  externals: ($) => [
    $._open_brace,
    $._close_brace,
    $._semi,
    $._raw_string,
    $._end_continuation_signal,
  ],
  extras: ($) => [/[ \t\r\n]/, $.linecomment, $.blockcomment],
  conflicts: ($) => [[$.ifexpr], [$.opexpr]],
  rules: {
    program: ($) =>
      seq(
        optional(seq(optional($._semis), "module", $.modulepath)),
        choice(
          seq(
            $._open_brace_,
            optional($._semis),
            optional($.declarations),
            $._close_brace_,
            optional($._semis),
          ),
          seq(optional($._semis), optional($.declarations)),
        ),
      ),
    eimport: ($) => seq($._IMPORT_EXTERN, $.externimpbody),
    import: ($) =>
      seq(
        optional($.pub),
        "import",
        $.modulepath,
        optional(seq("=", $.modulepath)),
      ),
    modulepath: ($) => choice($.varid, $.qvarid),
    pub: (_) => "pub",
    _semis: ($) => repeat1(alias($._semi, ";")),
    declarations: ($) =>
      repeat1(
        seq(choice($.import, $.eimport, $.fixitydecl, $.topdecl), $._semis),
      ),
    fixitydecl: ($) =>
      seq(
        optional($.pub),
        $.fixity,
        $.identifier,
        repeat(seq($._comma, $.identifier)),
      ),
    fixity: ($) =>
      choice(seq("infix", $.int), seq("infixr", $.int), seq("infixl", $.int)),
    topdecl: ($) =>
      choice(
        seq(optional($.pub), $.puredecl),
        seq(optional($.pub), $.aliasdecl),
        seq(optional($.pub), $.externdecl),
        seq(optional($.pub), $.typedecl),
        seq("abstract", $.typedecl),
      ),
    externdecl: ($) =>
      seq(
        optional($.inlinemod),
        optional($.fipmod),
        "extern",
        $.qidentifier,
        $.externtype,
        $.externbody,
      ),
    externtype: ($) =>
      choice(
        seq(":", $.typescheme),
        seq(
          optional($.typeparams),
          $._open_round_brace,
          optional($.parameters),
          ")",
          optional($.annotres),
        ),
      ),
    externbody: ($) =>
      choice(
        seq(
          $._open_brace_,
          optional($._semis),
          repeat1(seq($.externstat, $._semis)),
          $._close_brace_,
        ),
        seq($._open_brace_, optional($._semis), $._close_brace_),
      ),
    externstat: ($) =>
      choice(
        seq($.externtarget, optional($.externinline), $.string),
        seq(optional($.externinline), $.string),
      ),
    externinline: ($) => "inline",
    externimpbody: ($) =>
      choice(
        seq("=", $.externimp),
        seq(
          $._open_brace_,
          optional($._semis),
          repeat1(seq($.externimp, $._semis)),
          $._close_brace_,
        ),
      ),
    externimp: ($) =>
      choice(
        seq($.externtarget, $.varid, $.string),
        seq(
          $.externtarget,
          $._open_brace_,
          repeat1(seq($.externval, $._semis)),
          $._close_brace_,
        ),
      ),
    externval: ($) => seq($.varid, "=", $.string),
    externtarget: ($) => choice("cs", "js", "c"),
    aliasdecl: ($) =>
      seq(
        "alias",
        $.typeid,
        optional($.typeparams),
        optional($.kannot),
        "=",
        $.type,
      ),
    typedecl: ($) =>
      choice(
        field("type", seq(
          optional($.typemod),
          "type",
          $.typeid,
          optional($.typeparams),
          optional($.kannot),
          optional($.typebody),
        )),
        field("struct", seq(
          optional($.structmod),
          "struct",
          $.typeid,
          optional($.typeparams),
          optional($.kannot),
          optional($.conparams),
        )),
        field("effect_multiple", seq(
          optional($.effectmod),
          "effect",
          $.varid,
          optional($.typeparams),
          optional($.kannot),
          $.opdecls,
        )),
        field("effect_single", seq(
          optional($.effectmod),
          "effect",
          optional($.typeparams),
          optional($.kannot),
          $.operation,
        )),
        field("named_effect_multiple", seq(
          "named",
          optional($.effectmod),
          "effect",
          $.varid,
          optional($.typeparams),
          optional($.kannot),
          $.opdecls,
        )),
        field("named_effect_single", seq(
          "named",
          optional($.effectmod),
          "effect",
          optional($.typeparams),
          optional($.kannot),
          $.operation,
        )),
        seq(
          "named",
          optional($.effectmod),
          "effect",
          $.varid,
          optional($.typeparams),
          optional($.kannot),
          "in",
          $.type,
          $.opdecls,
        ),
      ),
    typemod: ($) => choice($.structmod, "open", "extend", "co", "div", "lazy"),
    structmod: ($) => choice("value", "reference", "ref"),
    effectmod: ($) => choice("div", "linear", seq("linear", "div")),
    typebody: ($) =>
      seq(
        $._open_brace_,
        optional($._semis),
        optional($.constructors),
        $._close_brace_,
      ),
    typeid: ($) =>
      choice(
        seq($._open_round_brace, optional($._commas), ")"),
        seq($._open_square_brace, "]"),
        seq($._open_angle_brace, ">"),
        seq($._open_angle_brace, "|", ">"),
        $.varid,
        "ctx",
      ),
    _commas: ($) => repeat1($._comma),
    _comma: ($) => prec.right(seq(",", optional($._end_continuation_signal))),
    constructors: ($) => repeat1(seq($.constructor, $._semis)),
    constructor: ($) =>
      choice(
        seq(
          optional($.pub),
          optional($.con),
          $.conid,
          optional($.typeparams),
          optional($.conparams),
        ),
        seq(
          optional($.pub),
          "lazy",
          optional($.con),
          $.conid,
          optional($.typeparams),
          optional($.conparams),
          "->",
          $._blockexpr,
        ),
      ),
    con: ($) => "con",
    conparams: ($) =>
      choice(
        seq($._open_round_brace, $.parameters, ")"),
        seq(
          $._open_brace_,
          optional($._semis),
          optional($.sconparams),
          $._close_brace_,
        ),
      ),
    sconparams: ($) =>
      seq($.parameter, $._semis, repeat(seq($.parameter, $._semis))),
    opdecls: ($) =>
      seq(
        $._open_brace_,
        optional($._semis),
        optional($.operations),
        $._close_brace_,
      ),
    operations: ($) =>
      seq($.operation, $._semis, repeat(seq($.operation, $._semis))),
    operation: ($) =>
      choice(
        seq(
          optional($.pub),
          "val",
          $.identifier,
          optional($.typeparams),
          ":",
          $.tatomic,
        ),
        seq(
          optional($.pub),
          "fun",
          $.identifier,
          optional($.typeparams),
          $._open_round_brace,
          optional($.pparameters),
          ")",
          ":",
          $.tatomic,
        ),
        seq(
          optional($.pub),
          optional($.controlmod),
          "ctl",
          $.identifier,
          optional($.typeparams),
          $._open_round_brace,
          optional($.pparameters),
          ")",
          ":",
          $.tatomic,
        ),
      ),
    puredecl: ($) =>
      choice(
        field(
          "val",
          seq(optional($.inlinemod), "val", $.binder, "=", $._blockexpr),
        ),
        field(
          "fun",
          seq(
            optional($.inlinemod),
            optional($.fipmod),
            "fun",
            $.qidentifier,
            $.funbody,
          ),
        ),
      ),
    inlinemod: ($) => choice("inline", "noinline"),
    fipmod: ($) =>
      choice(
        seq(optional($.tailmod), "fip", optional($.fiplimit)),
        seq(optional($.tailmod), "fbip", optional($.fiplimit)),
        $.tailmod,
      ),
    fiplimit: ($) =>
      choice(
        seq($._open_round_brace, $.int, ")"),
        seq($._open_round_brace, "_", ")"),
      ),
    tailmod: ($) => "tail",
    fundecl: ($) => seq($.identifier, $.funbody),
    binder: ($) => choice($.qidentifier, seq($.qidentifier, ":", $.type)),
    funbody: ($) =>
      choice(
        seq(
          optional($.typeparams),
          $._open_round_brace,
          optional($.pparameters),
          ")",
          $.bodyexpr,
        ),
        seq(
          optional($.typeparams),
          $._open_round_brace,
          optional($.pparameters),
          ")",
          ":",
          $.tresult,
          optional($.qualifier),
          $.block,
        ),
      ),
    annotres: ($) => seq(":", $.tresult),
    block: ($) =>
      seq(
        $._open_brace_,
        optional($._semis),
        repeat(seq($.statement, $._semis)),
        $._close_brace_,
      ),
    statement: ($) =>
      choice(
        $.decl,
        $.withstat,
        seq($.withstat, "in", $._blockexpr),
        $.returnexpr,
        $.basicexpr,
      ),
    decl: ($) =>
      choice(
        field("fun", seq("fun", $.fundecl)),
        field("val", seq("val", $._apattern, "=", $._blockexpr)),
        field("var", seq("var", $.binder, ":=", $._blockexpr)),
      ),
    bodyexpr: ($) => choice($._blockexpr, seq("->", $._blockexpr)),
    _blockexpr: ($) => $.expr,
    expr: ($) =>
      choice($.withexpr, $.block, $.returnexpr, $.valexpr, $.basicexpr),
    basicexpr: ($) =>
      choice($.ifexpr, $.matchexpr, $.handlerexpr, $.fnexpr, $.opexpr),
    matchexpr: ($) =>
      choice(
        seq(
          "match",
          $.ntlexpr,
          $._open_brace_,
          optional($._semis),
          optional($.matchrules),
          $._close_brace_,
        ),
        seq(
          "lazy",
          "match",
          $.ntlexpr,
          $._open_brace_,
          optional($._semis),
          optional($.matchrules),
          $._close_brace_,
        ),
      ),
    fnexpr: ($) => seq("fn", $.funbody),
    returnexpr: ($) => seq("return", $.expr),
    ifexpr: ($) =>
      choice(
        seq("if", $.ntlexpr, "then", $._blockexpr, optional($.elifs)),
        seq("if", $.ntlexpr, "return", $.expr),
      ),
    elifs: ($) =>
      seq(
        repeat(seq("elif", $.ntlexpr, "then", $._blockexpr)),
        "else",
        $._blockexpr,
      ),
    valexpr: ($) => seq("val", $._apattern, "=", $._blockexpr, "in", $.expr),
    opexpr: ($) =>
      sep1(
        seq(
          repeat(choice("!", "~")),
          $.atom,
          repeat(
            choice(
              field("call", seq($._open_round_brace, optional($.arguments), ")")),
              field("index", seq($._open_square_brace, optional($.arguments), "]")),
              field("dot", seq(".", $.name)),
              field("dotcall", seq(".", $._open_round_brace, optional($.arguments), ")")),
              field("trailing_lambda", $.block),
              field("trailing_lambda", $.fnexpr),
            ),
          ),
        ),
        $._qoperator,
      ),
    ntlexpr: ($) =>
      sep1(
        seq(
          repeat(choice("!", "~")),
          $.atom,
          repeat(
            choice(
              field("call", seq($._open_round_brace, optional($.arguments), ")")),
              field("index", seq($._open_square_brace, optional($.arguments), "]")),
              field("dot", seq(".", $.name)),
              field("dotcall", seq(".", $._open_round_brace, optional($.arguments), ")")),
            ),
          ),
        ),
        $._qoperator,
      ),
    atom: ($) =>
      choice(
        $.name,
        $.literal,
        $.mask,
        seq($._open_round_brace, optional($.aexprs), ")"),
        seq($._open_square_brace, optional($.cexprs), "]"),
        $.ctxexpr,
        $.ctxhole,
      ),
    name: ($) => choice($.qidentifier, $._qconstructor, $.qimplicit),
    literal: ($) => choice($.int, $.float, $.char, $.string),
    mask: ($) =>
      seq("mask", optional($.behindmod), $._open_angle_brace, $.tbasic, ">"),
    behindmod: ($) => "behind",
    ctxexpr: ($) => seq("ctx", $.atom),
    ctxhole: ($) => "_",
    arguments: ($) => seq($.argument, repeat(seq($._comma, $.argument))),
    argument: ($) =>
      choice(
        $.expr,
        seq($.identifier, "=", $.expr),
        seq($.qimplicit, "=", $.expr),
      ),
    parameters: ($) => seq($.parameter, repeat(seq($._comma, $.parameter))),
    parameter: ($) =>
      choice(
        seq(optional($.borrow), optional($.pub), $.paramid, ":", $.type),
        seq(
          optional($.borrow),
          optional($.pub),
          $.paramid,
          ":",
          $.type,
          "=",
          $.expr,
        ),
      ),
    paramid: ($) => choice($.identifier, $.wildcard),
    borrow: ($) => "^",
    pparameters: ($) => seq($.pparameter, repeat(seq($._comma, $.pparameter))),
    pparameter: ($) =>
      choice(
        seq(optional($.borrow), $.pattern),
        seq(optional($.borrow), $.pattern, ":", $.type),
        seq(optional($.borrow), $.pattern, ":", $.type, "=", $.expr),
        seq(optional($.borrow), $.pattern, "=", $.expr),
        seq(optional($.borrow), $.qimplicit),
        seq(optional($.borrow), $.qimplicit, ":", $.type),
      ),
    aexprs: ($) =>
      seq($._aexpr, repeat(seq($._comma, $._aexpr)), optional($._comma)),
    cexprs: ($) =>
      seq($._aexpr, repeat(seq($._comma, $._aexpr)), optional($._comma)),
    _aexpr: ($) => seq($.expr, optional($.annot)),
    annot: ($) => seq(":", $.typescheme),
    _qoperator: ($) => $.op,
    qidentifier: ($) => choice($.qvarid, $.qidop, $.identifier),
    identifier: ($) => choice($.varid, $.idop),
    wildcard: ($) => choice($._WILDCARDID, "_"),
    qimplicit: ($) => $._IMPLICITID,
    qvarid: ($) => $.qid,
    varid: ($) =>
      choice(
        $.id,
        "c",
        "cs",
        "js",
        "file",
        "inline",
        "noinline",
        "open",
        "extend",
        "linear",
        "behind",
        "value",
        "reference",
        "scoped",
        "initially",
        "finally",
        "div",
        "co",
        "fip",
        "fbip",
        "tail",
        "lazy",
      ),
    _qconstructor: ($) => choice($.conid, $.qconid),
    qconid: ($) => $.qconid,
    conid: ($) => $.conid,
    op: ($) => choice($.OP, ">", $._open_angle_brace, "|", ":="),
    matchrules: ($) => repeat1(seq($.matchrule, $._semis)),
    matchrule: ($) =>
      choice(
        seq($.patterns, "|", $.expr, "->", $._blockexpr),
        seq($.patterns, "->", $._blockexpr),
      ),
    patterns: ($) => seq($.pattern, repeat(seq($._comma, $.pattern))),
    apatterns: ($) => seq($._apattern, repeat(seq($._comma, $._apattern))),
    _apattern: ($) => seq($.pattern, optional($.annot)),
    pattern: ($) =>
      seq(
        repeat(seq($.identifier, "as")),
        choice(
          $.identifier,
          field("constructor", $.conid),
          field("constructor", seq($.conid, $._open_round_brace, optional($.patargs), ")")),
          field("tuple", seq($._open_round_brace, optional($.apatterns), ")")),
          field("list", seq($._open_square_brace, optional($.apatterns), "]")),
          $.literal,
          $.wildcard,
        ),
      ),
    patargs: ($) =>
      choice(seq(optional($.patargs), $._comma, $.patarg), $.patarg),
    patarg: ($) => choice(seq($.identifier, "=", $._apattern), $._apattern),
    handlerexpr: ($) =>
      choice(
        seq(optional($.override), "handler", optional($.witheff), $.opclauses),
        seq(
          optional($.override),
          "handle",
          optional($.witheff),
          $.ntlexpr,
          $.opclauses,
        ),
        seq("named", "handler", optional($.witheff), $.opclauses),
        seq("named", "handle", optional($.witheff), $.ntlexpr, $.opclauses),
      ),
    override: ($) => "override",
    witheff: ($) => seq($._open_angle_brace, $._anntype, ">"),
    withstat: ($) =>
      choice(
        seq("with", $.basicexpr),
        seq("with", $.binder, "<-", $.basicexpr),
        seq("with", optional($.override), optional($.witheff), $.opclause),
        seq("with", $.binder, "<-", optional($.witheff), $.opclause),
      ),
    withexpr: ($) => seq($.withstat, "in", $._blockexpr),
    opclauses: ($) =>
      choice(
        seq(
          $._open_brace_,
          optional($._semis),
          repeat1(seq($.opclausex, $._semis)),
          $._close_brace_,
        ),
        seq($._open_brace_, optional($._semis), $._close_brace_),
      ),
    opclausex: ($) =>
      choice(
        seq("finally", $.bodyexpr),
        seq("initially", $._open_round_brace, $.opparam, ")", $.bodyexpr),
        $.opclause,
      ),
    opclause: ($) =>
      choice(
        seq("val", $.qidentifier, "=", $._blockexpr),
       seq("val", $.qidentifier, ":", $.type, "=", $._blockexpr),
        seq("fun", $.qidentifier, $.opparams, $.bodyexpr),
        seq(
          choice(
            seq(optional($.controlmod), "ctl"),
            "brk", // Deprecated
          ),
          $.qidentifier,
          $.opparams,
          $.bodyexpr,
        ),
        seq("return", $._open_round_brace, $.opparam, ")", $.bodyexpr),
      ),
    controlmod: ($) => choice("final", "raw"),
    opparams: ($) =>
      seq(
        $._open_round_brace,
        $.opparam,
        repeat(seq($._comma, $.opparam)),
        ")",
      ),
    opparam: ($) => choice($.paramid, seq($.paramid, ":", $.type)),
    tbinder: ($) => seq($.varid, optional($.kannot)),
    typescheme: ($) =>
      seq(optional($.someforalls), $._tarrow, optional($.qualifier)),
    type: ($) =>
      choice(
        seq("forall", $.typeparams, $._tarrow, optional($.qualifier)),
        seq($._tarrow, optional($.qualifier)),
      ),
    someforalls: ($) =>
      choice(
        seq("some", $.typeparams, "forall", $.typeparams),
        seq("some", $.typeparams),
        seq("forall", $.typeparams),
      ),
    typeparams: ($) =>
      seq(
        $._open_angle_brace,
        optional(seq($.tbinder, repeat(seq($._comma, $.tbinder)))),
        ">",
      ),
    qualifier: ($) =>
      seq(
        "with",
        $._open_round_brace,
        $.predicate,
        repeat(seq($._comma, $.predicate)),
        ")",
      ),
    predicate: ($) => $._typeapp,
    _tarrow: ($) => choice(seq($.tatomic, "->", $.tresult), $.tatomic),
    tresult: ($) => choice(seq($.tatomic, $.tbasic), $.tatomic),
    tatomic: ($) =>
      choice(
        $.tbasic,
        seq($._open_angle_brace, $.targuments, "|", $.tatomic, ">"),
        seq($._open_angle_brace, optional($.targuments), ">"),
      ),
    tbasic: ($) =>
      choice(
        $._typeapp,
        seq($._open_round_brace, optional($.tparams), ")"),
        seq($._open_square_brace, $._anntype, "]"),
      ),
    _typeapp: ($) =>
      choice(
        $.typecon,
        seq($.typecon, $._open_angle_brace, optional($.targuments), ">"),
      ),
    typecon: ($) =>
      choice(
        $.varid,
        $.qvarid,
        $.wildcard,
        seq($._open_round_brace, $._commas, ")"),
        seq($._open_square_brace, "]"),
        seq($._open_round_brace, "->", ")"),
        "ctx",
      ),
    tparams: ($) => seq($.tparam, repeat(seq($._comma, $.tparam))),
    tparam: ($) => choice(seq($.identifier, ":", $._anntype), $._anntype),
    targuments: ($) => seq($._anntype, repeat(seq($._comma, $._anntype))),
    _anntype: ($) => seq($.type, optional($.kannot)),
    kannot: ($) => seq("::", $.kind),
    kind: ($) =>
      seq(
        repeat(seq($.katom, "->")),
        choice(
          seq(
            $._open_round_brace,
            $.kind,
            repeat(seq($._comma, $.kind)),
            ")",
            "->",
            $.katom,
          ),
          $.katom,
        ),
      ),
    katom: ($) => $.conid,
    _open_brace_: ($) => alias($._open_brace, "{"),
    _close_brace_: ($) => alias($._close_brace, "}"),
    _open_square_brace: ($) =>
      prec.right(seq("[", optional($._end_continuation_signal))),
    _open_angle_brace: ($) =>
      prec.right(seq("<", optional($._end_continuation_signal))),
    _open_round_brace: ($) =>
      prec.right(seq("(", optional($._end_continuation_signal))),
    _IMPORT_EXTERN: ($) =>
      choice(seq("import", "extern"), seq("extern", "import")),
    // Character classes
    _symbols: (_) => /[$%&*+@!\\^~=.\-:?|<>]+|\//,
    conid: (_) => /@?[A-Z][a-zA-Z0-9_-]*'*/,
    id: (_) => /@?[a-z][a-zA-Z0-9_-]*'*/,
    escape: (_) =>
      token.immediate(
        /\\([nrt\\"']|x[0-9a-fA-F]{2}|u[0-9a-fA-F]{4}|U[0-9a-fA-F]{6})/,
      ),

    // Comments
    linecomment: (_) => token(seq("//", /.*/)),
    blockcomment: ($) =>
      seq("/*", repeat(choice(/[^*]|\*[^/]/, $.blockcomment)), "*/"),

    // Numbers
    float: (_) =>
      choice(
        /-?(0|[1-9](_?[0-9]+(_[0-9]+)*)?)((\.[0-9]+(_[0-9]+)*)?[eE][-+]?[0-9]+|\.[0-9]+(_[0-9]+)*)/,
        /-?0[xX][0-9a-fA-F]+(_[0-9a-fA-F]+)*((\.[0-9a-fA-F]+(_[0-9a-fA-F]+)*)?[pP][-+]?[0-9]+|\.[0-9a-fA-F]+(_[0-9a-fA-F]+)*)/,
      ),
    int: (_) =>
      choice(
        /-?(0|[1-9](_?[0-9]+(_[0-9]+)*)?)/,
        /-?0[xX][0-9a-fA-F]+(_[0-9a-fA-F]+)*/,
      ),

    // Identifiers and operators
    qconid: (_) => /([a-z][a-zA-Z0-9_-]*'*\/)+@?[A-Z][a-zA-Z0-9_-]*'*/,
    qid: (_) => /([a-z][a-zA-Z0-9_-]*'*\/)+@?[a-z][a-zA-Z0-9_-]*'*/,
    qidop: (_) => /([a-z][a-zA-Z0-9_-]*'*\/)+\(([$%&*+@!\\^~=.\-:?|<>]+|\/)\)/,
    idop: (_) =>
      seq(
        "(",
        token.immediate(/[$%&*+@!\\^~=.\-:?|<>]+|\//),
        token.immediate(")"),
      ),
    OP: ($) =>
      prec.right(seq($._symbols, optional($._end_continuation_signal))),
    _WILDCARDID: (_) => /_[a-zA-Z0-9_-]*/,
    _IMPLICITID: ($) => seq("?", choice($.qid, $.qidop, $.id, $.idop)),

    // String
    string: ($) =>
      choice(
        $._raw_string,
        seq(
          '"',
          repeat(choice(token.immediate(/[^\\"]+/), $.escape)),
          token.immediate('"'),
        ),
      ),
    char: ($) =>
      seq(
        "'",
        choice(token.immediate(/[^\\']/), $.escape),
        token.immediate("'"),
      ),
  },
});

/**
 * Creates a rule to match one or more of the rules separated by a seperator
 *
 * @param {Rule} rule
 * @param {Rule} sep
 *
 * @return {SeqRule}
 *
 */
function sep1(rule, sep) {
  return seq(rule, repeat(seq(sep, rule)));
}
