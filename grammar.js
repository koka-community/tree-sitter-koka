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
  conflicts: ($) => [
    [$.constructors1],
    [$.cexprs0],
    [$.opclauses1],
    [$.matchrules1],
    [$.ifexpr],
    [$.opexpr],
  ],
  rules: {
    program: ($) =>
      seq(
        optional(seq(optional($.semis), "module", $.modulepath)),
        choice(
          seq(
            $._open_brace_,
            optional($.semis),
            optional($.declarations),
            $._close_brace_,
            optional($.semis),
          ),
          seq(optional($.semis), optional($.declarations)),
        ),
      ),
    import: ($) => $.importdecl,
    eimport: ($) => seq($.IMPORT_EXTERN, $.externimpbody),
    importdecl: ($) =>
      seq(
        optional($.pub),
        "import",
        $.modulepath,
        optional(seq("=", $.modulepath)),
      ),
    modulepath: ($) => choice($.varid, $.qvarid),
    pub: ($) => "pub",
    semis: ($) => repeat1(alias($._semi, ";")),
    declarations: ($) =>
      repeat1(
        seq(choice($.import, $.eimport, $.fixitydecl, $.topdecl), $.semis),
      ),
    fixitydecl: ($) => seq(optional($.pub), $.fixity, $.oplist1),
    fixity: ($) =>
      choice(seq("infix", $.int), seq("infixr", $.int), seq("infixl", $.int)),
    oplist1: ($) => seq($.identifier, repeat(seq(",", $.identifier))),
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
          "(",
          optional($.parameters),
          ")",
          optional($.annotres),
        ),
      ),
    externbody: ($) =>
      choice(
        seq($._open_brace_, optional($.semis), $.externstats1, $._close_brace_),
        seq($._open_brace_, optional($.semis), $._close_brace_),
      ),
    externstats1: ($) =>
      seq($.externstat, $.semis, repeat(seq($.externstat, $.semis))),
    externstat: ($) =>
      choice(
        seq($.externtarget, optional($.externinline), $.string),
        seq(optional($.externinline), $.string),
      ),
    externinline: ($) => "inline",
    externimpbody: ($) =>
      choice(
        seq("=", $.externimp),
        seq($._open_brace_, optional($.semis), $.externimps1, $._close_brace_),
      ),
    externimps1: ($) =>
      seq($.externimp, $.semis, repeat(seq($.externimp, $.semis))),
    externimp: ($) =>
      choice(
        seq($.externtarget, $.varid, $.string),
        seq($.externtarget, $._open_brace_, $.externvals1, $._close_brace_),
      ),
    externvals1: ($) =>
      seq($.externval, $.semis, repeat(seq($.externval, $.semis))),
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
        seq(
          optional($.typemod),
          "type",
          $.typeid,
          optional($.typeparams),
          optional($.kannot),
          optional($.typebody),
        ),
        seq(
          optional($.structmod),
          "struct",
          $.typeid,
          optional($.typeparams),
          optional($.kannot),
          optional($.conparams),
        ),
        seq(
          optional($.effectmod),
          "effect",
          $.varid,
          optional($.typeparams),
          optional($.kannot),
          $.opdecls,
        ),
        seq(
          optional($.effectmod),
          "effect",
          optional($.typeparams),
          optional($.kannot),
          $.operation,
        ),
        seq(
          "named",
          optional($.effectmod),
          "effect",
          $.varid,
          optional($.typeparams),
          optional($.kannot),
          $.opdecls,
        ),
        seq(
          "named",
          optional($.effectmod),
          "effect",
          optional($.typeparams),
          optional($.kannot),
          $.operation,
        ),
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
    structmod: ($) => choice("value", "reference"),
    effectmod: ($) => choice("div", "linear", seq("linear", "div")),
    typebody: ($) =>
      seq(
        $._open_brace_,
        optional($.semis),
        optional($.constructors),
        $._close_brace_,
      ),
    typeid: ($) =>
      choice(
        seq("(", optional($.commas), ")"),
        seq("[", "]"),
        seq("<", ">"),
        seq("<", "|", ">"),
        $.varid,
        "ctx",
      ),
    commas: ($) => $.commas1,
    commas1: ($) => seq(optional($.commas), ","),
    constructors: ($) => seq($.constructors1, $.semis),
    constructors1: ($) =>
      seq($.constructor, repeat(seq($.semis, $.constructor))),
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
          $.blockexpr,
        ),
      ),
    con: ($) => "con",
    conparams: ($) =>
      choice(
        seq("(", $.parameters1, ")"),
        seq(
          $._open_brace_,
          optional($.semis),
          optional($.sconparams),
          $._close_brace_,
        ),
      ),
    sconparams: ($) =>
      seq($.parameter, $.semis, repeat(seq($.parameter, $.semis))),
    opdecls: ($) =>
      seq(
        $._open_brace_,
        optional($.semis),
        optional($.operations),
        $._close_brace_,
      ),
    operations: ($) =>
      seq($.operation, $.semis, repeat(seq($.operation, $.semis))),
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
          "(",
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
          "(",
          optional($.pparameters),
          ")",
          ":",
          $.tatomic,
        ),
      ),
    puredecl: ($) =>
      choice(
        seq(optional($.inlinemod), "val", $.binder, "=", $.blockexpr),
        seq(
          optional($.inlinemod),
          optional($.fipmod),
          "fun",
          $.qidentifier,
          $.funbody,
        ),
      ),
    inlinemod: ($) => choice("inline", "noinline"),
    fipmod: ($) =>
      choice(
        seq(optional($.tailmod), "fip", optional($.fiplimit)),
        seq(optional($.tailmod), "fbip", optional($.fiplimit)),
        $.tailmod,
      ),
    fiplimit: ($) => choice(seq("(", $.int, ")"), seq("(", "_", ")")),
    tailmod: ($) => "tail",
    fundecl: ($) => seq($.identifier, $.funbody),
    binder: ($) => choice($.identifier, seq($.identifier, ":", $.type)),
    funbody: ($) =>
      choice(
        seq(
          optional($.typeparams),
          "(",
          optional($.pparameters),
          ")",
          $.bodyexpr,
        ),
        seq(
          optional($.typeparams),
          "(",
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
        optional($.semis),
        repeat(seq($.statement, $.semis)),
        $._close_brace_,
      ),
    statement: ($) =>
      choice(
        $.decl,
        $.withstat,
        seq($.withstat, "in", $.blockexpr),
        $.returnexpr,
        $.basicexpr,
      ),
    decl: ($) =>
      choice(
        seq("fun", $.fundecl),
        seq("val", $.apattern, "=", $.blockexpr),
        seq("var", $.binder, ":=", $.blockexpr),
      ),
    bodyexpr: ($) => choice($.blockexpr, seq("->", $.blockexpr)),
    blockexpr: ($) => $.expr,
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
          optional($.semis),
          optional($.matchrules),
          $._close_brace_,
        ),
        seq(
          "lazy",
          "match",
          $.ntlexpr,
          $._open_brace_,
          optional($.semis),
          optional($.matchrules),
          $._close_brace_,
        ),
      ),
    fnexpr: ($) => seq("fn", $.funbody),
    returnexpr: ($) => seq("return", $.expr),
    ifexpr: ($) =>
      choice(
        seq("if", $.ntlexpr, "then", $.blockexpr, $.elifs),
        seq("if", $.ntlexpr, "then", $.blockexpr),
        seq("if", $.ntlexpr, "return", $.expr),
      ),
    elifs: ($) =>
      seq(
        repeat(seq("elif", $.ntlexpr, "then", $.blockexpr)),
        "else",
        $.blockexpr,
      ),
    valexpr: ($) => seq("val", $.apattern, "=", $.blockexpr, "in", $.expr),
    opexpr: ($) =>
      sep1(
        seq(
          repeat(choice("!", "~")),
          $.atom,
          repeat(
            choice(
              seq("(", optional($.arguments), ")"),
              seq("[", optional($.arguments), "]"),
              seq(".", $.name),
              seq(".", "(", optional($.arguments), ")"),
              $.block,
              $.fnexpr,
            ),
          ),
        ),
        $.qoperator,
      ),
    ntlexpr: ($) =>
      sep1(
        seq(
          repeat(choice("!", "~")),
          $.atom,
          repeat(
            choice(
              seq("(", optional($.arguments), ")"),
              seq("[", optional($.arguments), "]"),
              seq(".", $.name),
              seq(".", "(", optional($.arguments), ")"),
            ),
          ),
        ),
        $.qoperator,
      ),
    atom: ($) =>
      choice(
        $.name,
        $.literal,
        $.mask,
        seq("(", optional($.aexprs), ")"),
        seq("[", optional($.cexprs), "]"),
        $.ctxexpr,
        $.ctxhole,
      ),
    name: ($) => choice($.qidentifier, $.qconstructor, $.qimplicit),
    literal: ($) => choice($.int, $.float, $.char, $.string),
    mask: ($) => seq("mask", optional($.behind), "<", $.tbasic, ">"),
    behind: ($) => "behind",
    ctxexpr: ($) => seq("ctx", $.atom),
    ctxhole: ($) => "_",
    arguments: ($) => $.arguments1,
    arguments1: ($) => seq($.argument, repeat(seq(",", $.argument))),
    argument: ($) =>
      choice(
        $.expr,
        seq($.identifier, "=", $.expr),
        seq($.qimplicit, "=", $.expr),
      ),
    parameters: ($) => $.parameters1,
    parameters1: ($) => seq($.parameter, repeat(seq(",", $.parameter))),
    parameter: ($) =>
      choice(
        seq(optional($.borrow), $.paramid, ":", $.type),
        seq(optional($.borrow), $.paramid, ":", $.type, "=", $.expr),
      ),
    paramid: ($) => choice($.identifier, $.wildcard),
    borrow: ($) => "^",
    pparameters: ($) => $.pparameters1,
    pparameters1: ($) => seq($.pparameter, repeat(seq(",", $.pparameter))),
    pparameter: ($) =>
      choice(
        field("a", seq(optional($.borrow), $.pattern)),
        field("b", seq(optional($.borrow), $.pattern, ":", $.type)),
        field(
          "c",
          seq(optional($.borrow), $.pattern, ":", $.type, "=", $.expr),
        ),
        field("d", seq(optional($.borrow), $.pattern, "=", $.expr)),
        field("e", seq(optional($.borrow), $.qimplicit)),
        field("f", seq(optional($.borrow), $.qimplicit, ":", $.type)),
      ),
    aexprs: ($) => seq($.aexpr, repeat(seq(",", $.aexpr))),
    cexprs: ($) => choice($.cexprs0, seq(optional($.cexprs0), $.aexpr)),
    cexprs0: ($) => seq($.aexpr, ",", repeat(seq($.aexpr, ","))),
    aexpr: ($) => seq($.expr, optional($.annot)),
    annot: ($) => seq(":", $.typescheme),
    qoperator: ($) => $.op,
    qidentifier: ($) => choice($.qvarid, $.QIDOP, $.identifier),
    identifier: ($) => choice($.varid, $.IDOP),
    wildcard: ($) => choice($.WILDCARDID, "_"),
    qimplicit: ($) => $.IMPLICITID,
    qvarid: ($) => $.QID,
    varid: ($) =>
      choice(
        $.ID,
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
    qconstructor: ($) => choice($.conid, $.qconid),
    qconid: ($) => $.QCONID,
    conid: ($) => $.CONID,
    op: ($) => choice($._OP, ">", "<", "|", ":="),
    matchrules: ($) => seq($.matchrules1, $.semis),
    matchrules1: ($) => seq($.matchrule, repeat(seq($.semis, $.matchrule))),
    matchrule: ($) =>
      choice(
        seq($.patterns1, "|", $.expr, "->", $.blockexpr),
        seq($.patterns1, "->", $.blockexpr),
      ),
    patterns1: ($) => seq($.pattern, repeat(seq(",", $.pattern))),
    apatterns: ($) => $.apatterns1,
    apatterns1: ($) => seq($.apattern, repeat(seq(",", $.apattern))),
    apattern: ($) => seq($.pattern, optional($.annot)),
    pattern: ($) =>
      seq(
        repeat(seq($.identifier, "as")),
        choice(
          $.identifier,
          $.conid,
          seq($.conid, "(", optional($.patargs), ")"),
          seq("(", optional($.apatterns), ")"),
          seq("[", optional($.apatterns), "]"),
          $.literal,
          $.wildcard,
        ),
      ),
    patargs: ($) => $.patargs1,
    patargs1: ($) => choice(seq(optional($.patargs), ",", $.patarg), $.patarg),
    patarg: ($) => choice(seq($.identifier, "=", $.apattern), $.apattern),
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
    witheff: ($) => seq("<", $.anntype, ">"),
    withstat: ($) =>
      choice(
        seq("with", $.basicexpr),
        seq("with", $.binder, "<-", $.basicexpr),
        seq("with", optional($.override), optional($.witheff), $.opclause),
        seq("with", $.binder, "<-", optional($.witheff), $.opclause),
      ),
    withexpr: ($) => seq($.withstat, "in", $.blockexpr),
    opclauses: ($) =>
      choice(
        seq(
          $._open_brace_,
          optional($.semis),
          $.opclauses1,
          $.semis,
          $._close_brace_,
        ),
        seq($._open_brace_, optional($.semis), $._close_brace_),
      ),
    opclauses1: ($) => seq($.opclausex, repeat(seq($.semis, $.opclausex))),
    opclausex: ($) =>
      choice(
        seq("finally", $.bodyexpr),
        seq("initially", "(", $.opparam, ")", $.bodyexpr),
        $.opclause,
      ),
    opclause: ($) =>
      choice(
        seq("val", $.qidentifier, "=", $.blockexpr),
        seq("val", $.qidentifier, ":", $.type, "=", $.blockexpr),
        seq("fun", $.qidentifier, $.opparams, $.bodyexpr),
        seq(
          optional($.controlmod),
          "ctl",
          $.qidentifier,
          $.opparams,
          $.bodyexpr,
        ),
        seq("return", "(", $.opparam, ")", $.bodyexpr),
      ),
    controlmod: ($) => choice("final", "raw"),
    opparams: ($) => seq("(", optional($.opparams0), ")"),
    opparams0: ($) => $.opparams1,
    opparams1: ($) => seq($.opparam, repeat(seq(",", $.opparam))),
    opparam: ($) => choice($.paramid, seq($.paramid, ":", $.type)),
    tbinders: ($) => $.tbinders1,
    tbinders1: ($) => seq($.tbinder, repeat(seq(",", $.tbinder))),
    tbinder: ($) => seq($.varid, optional($.kannot)),
    typescheme: ($) =>
      seq(optional($.someforalls), $.tarrow, optional($.qualifier)),
    type: ($) =>
      choice(
        seq("forall", $.typeparams1, $.tarrow, optional($.qualifier)),
        seq($.tarrow, optional($.qualifier)),
      ),
    someforalls: ($) =>
      choice(
        seq("some", $.typeparams1, "forall", $.typeparams1),
        seq("some", $.typeparams1),
        seq("forall", $.typeparams1),
      ),
    typeparams: ($) => $.typeparams1,
    typeparams1: ($) => seq("<", optional($.tbinders), ">"),
    qualifier: ($) => seq("with", "(", $.predicates1, ")"),
    predicates1: ($) => seq($.predicate, repeat(seq(",", $.predicate))),
    predicate: ($) => $.typeapp,
    tarrow: ($) => choice(seq($.tatomic, "->", $.tresult), $.tatomic),
    tresult: ($) => choice(seq($.tatomic, $.tbasic), $.tatomic),
    tatomic: ($) =>
      choice(
        $.tbasic,
        seq("<", $.targuments1, "|", $.tatomic, ">"),
        seq("<", optional($.targuments), ">"),
      ),
    tbasic: ($) =>
      choice(
        $.typeapp,
        seq("(", optional($.tparams), ")"),
        seq("[", $.anntype, "]"),
      ),
    typeapp: ($) =>
      choice($.typecon, seq($.typecon, "<", optional($.targuments), ">")),
    typecon: ($) =>
      choice(
        $.varid,
        $.qvarid,
        $.wildcard,
        seq("(", $.commas1, ")"),
        seq("[", "]"),
        seq("(", "->", ")"),
        "ctx",
      ),
    tparams: ($) => $.tparams1,
    tparams1: ($) => seq($.tparam, repeat(seq(",", $.tparam))),
    tparam: ($) => choice(seq($.identifier, ":", $.anntype), $.anntype),
    targuments: ($) => $.targuments1,
    targuments1: ($) => seq($.anntype, repeat(seq(",", $.anntype))),
    anntype: ($) => seq($.type, optional($.kannot)),
    kannot: ($) => seq("::", $.kind),
    kind: ($) =>
      seq(
        repeat(seq($.katom, "->")),
        choice(seq("(", $.kinds1, ")", "->", $.katom), $.katom),
      ),
    kinds1: ($) => seq($.kind, repeat(seq(",", $.kind))),
    katom: ($) => $.conid,
    // =================
    // CUSTOM DEFINITIONS
    // =================
    _open_brace_: ($) => alias($._open_brace, "{"),
    _close_brace_: ($) => alias($._close_brace, "}"),
    IMPORT_EXTERN: ($) =>
      choice(seq("import", "extern"), seq("extern", "import")),
    // Character classes
    _symbols: (_) => /[$%&*+@!\\^~=.\-:?|<>]+|\//,
    CONID: (_) => /[A-Z][a-zA-Z0-9_-]*'*/,
    ID: (_) => /[a-z][a-zA-Z0-9_-]*'*/,
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
    QCONID: (_) => /([a-z][a-zA-Z0-9_-]*'*\/)+[A-Z][a-zA-Z0-9_-]*'*/,
    QID: (_) => /([a-z][a-zA-Z0-9_-]*'*\/)+[a-z][a-zA-Z0-9_-]*'*/,
    QIDOP: (_) => /([a-z][a-zA-Z0-9_-]*'*\/)+\(([$%&*+@!\\^~=.\-:?|<>]+|\/)\)/,
    IDOP: (_) =>
      seq(
        "(",
        token.immediate(/[$%&*+@!\\^~=.\-:?|<>]+|\//),
        token.immediate(")"),
      ),
    _OP: ($) =>
      prec.right(seq($._symbols, optional($._end_continuation_signal))),
    WILDCARDID: (_) => /_[a-zA-Z0-9_-]*/,
    IMPLICITID: ($) => seq("?", choice($.QID, $.QIDOP)),

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
