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
  conflicts: ($) => [
    // Context-free syntax doesn't specify operator precedences.
    [$.prefixexpr, $.appexpr],
    // Necessary for allowing statements at the top level, which we want to do
    // so this can be used to highlight code blocks.
    [$.binder, $.pattern],
    [$.puredecl, $.fundecl],
  ],
  word: ($) => $.id,
  rules : {
    // =======
    // Program
    // =======
    program: ($) => choice(
      seq($.semis, "module", $.modulepath, $.moduledecl,),
      $.moduledecl,
    ),
    moduledecl: ($) => choice(
      seq($._open_brace, $.semis, $.imports, $._close_brace, $.semis,),
      seq($.semis, $.imports,),
    ),
    imports: ($) => choice(
      seq($.importdecl, $.semis1, $.imports,),
      $.eimports,
    ),
    eimports: ($) => choice(
      seq($.IMPORT_EXTERN, $.externimpbody, $.semis1, $.eimports),
      $.declarations,
    ),
    importdecl: ($) => choice(
      seq($.pub, "import", $.modulepath)
      seq($.pub, "import", $.modulepath, "=", $.modulepath)
    ),
    modulepath: ($) => choice(
      $.varid
      $.qvarid
    ),
    pub: ($) => optional("pub"),
    semis1: ($) => repeat1(alias($._semi, ";")),
    semis: ($) => repeat(alias($._semi, ";")),
    // ======================
    // Top level declarations
    // ======================
    declarations: ($) => $.fixitydecl
    // =================
    // TOKEN DEFINITIONS
    // =================
    IMPORT_EXTERN: ($) => choice(
      seq("import", "extern"),
      seq("extern", "import"),
    )
  },
  rules: {
    // Program
    program: ($) =>
      choice(
        seq(
          optional(seq(optional($._semis), "module", $.modulepath)),
          alias(
            choice(
              seq(
                $._open_brace_,
                optional($._semis),
                alias(
                  seq(
                    repeat(seq($.importdecl, $._semis)),
                    repeat(seq($.fixitydecl, $._semis)),
                    optional($._topdecls),
                  ),
                  $.modulebody,
                ),
                $._close_brace_,
                optional($._semis),
              ),
              seq(
                optional($._semis),
                alias(
                  seq(
                    repeat(seq($.importdecl, $._semis)),
                    repeat(seq($.fixitydecl, $._semis)),
                    optional($._topdecls),
                  ),
                  $.modulebody,
                ),
              ),
            ),
            $.moduledecl,
          ),
        ),
        prec(-1, seq(optional($._semis), $.statements)),
      ),
    // So syntax highlighting of literal '{' will work without having to make
    // the externals named nodes in the tree.
    _open_brace_: ($) => alias($._open_brace, "{"),
    _close_brace_: ($) => alias($._close_brace, "}"),
    importdecl: ($) =>
      seq(
        optional(choice("pub", "public")),
        "import",
        $.modulepath,
        optional(seq("=", $.modulepath)),
      ),
    modulepath: ($) => choice($.varid, $.qvarid),
    _semis: ($) => repeat1(alias($._semi, ";")),

    // Top level declarations
    fixitydecl: ($) => seq(optional("pub"), $.fixity, $.oplist),
    fixity: ($) => seq(choice("infix", "infixl", "infixr"), $.int),
    oplist: ($) => sep1($.identifier, $._comma),
    _topdecls: ($) => repeat1(seq($.topdecl, $._semis)),
    topdecl: ($) =>
      choice(
        seq(
          optional("pub"),
          choice($.puredecl, $.aliasdecl, $.externdecl, $.typedecl),
        ),
        seq("abstract", $.typedecl),
      ),

    // External declarations
    externdecl: ($) =>
      choice(
        seq(
          choice("inline", "noinline"),
          optional($.fipmod),
          "extern",
          $.funid,
          $.externtype,
          $.externbody,
        ),
        seq(optional($.fipmod), "extern", $.funid, $.externtype, $.externbody),
        seq(
          "extern",
          choice(
            "import",
            "include", // Deprecated, but still supported.
          ),
          $.externimpbody,
        ),
      ),
    _open_round_brace: ($) =>
      prec.right(seq("(", optional($._end_continuation_signal))),
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
      seq(
        $._open_brace_,
        optional($._semis),
        repeat(seq($.externstat, $._semis)),
        $._close_brace_,
      ),
    externstat: ($) =>
      seq(optional($.externtarget), optional("inline"), $.string),
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
    externtarget: (_) => choice("cs", "js", "c"),

    // Type declarations
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
          optional("named"),
          optional("scoped"),
          optional($.effectmod),
          "effect",
          $.varid,
          optional($.typeparams),
          optional($.kannot),
          $.opdecls,
        ),
        seq(
          optional("named"),
          optional("scoped"),
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
    typemod: ($) => choice($.structmod, "open", "extend", "co", "rec"),
    structmod: (_) => choice("value", "ref", "reference"),
    effectmod: (_) => choice("rec", "linear", seq("linear", "rec")),
    typebody: ($) =>
      seq(
        $._open_brace_,
        optional($._semis),
        optional($.constructors),
        $._close_brace_,
      ),
    _open_square_brace: ($) =>
      prec.right(seq("[", optional($._end_continuation_signal))),
    _open_angle_brace: ($) =>
      prec.right(seq("<", optional($._end_continuation_signal))),
    typeid: ($) =>
      choice(
        seq($._open_round_brace, optional($.commas), ")"),
        seq($._open_square_brace, "]"),
        seq($._open_angle_brace, ">"),
        seq($._open_angle_brace, "|", ">"),
        $.varid,
      ),
    _comma: ($) => prec.right(seq(",", optional($._end_continuation_signal))),
    commas: ($) => repeat1($._comma),
    constructors: ($) => repeat1(seq($.constructor, $._semis)),
    constructor: ($) =>
      seq(
        optional("pub"),
        optional("con"),
        choice($.conid, $.string),
        optional($.typeparams),
        optional($.conparams),
      ),
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
    sconparams: ($) => repeat1(seq($.parameter, $._semis)),

    // Effect declarations
    opdecls: ($) =>
      seq(
        $._open_brace_,
        optional($._semis),
        optional($.operations),
        $._close_brace_,
      ),
    operations: ($) => repeat1(seq($.operation, $._semis)),
    operation: ($) =>
      choice(
        seq(
          optional("pub"),
          "val",
          $.identifier,
          optional($.typeparams),
          ":",
          $.tatomic,
        ),
        seq(
          optional("pub"),
          choice(
            "fun",
            "control",
            "rcontrol",
            "rawctl",
            seq(optional($.controlmod), "ctl"),
          ),
          $.identifier,
          optional($.typeparams),
          $._open_round_brace,
          optional($.parameters),
          ")",
          ":",
          $.tatomic,
        ),
      ),

    // Pure (top-level) Declarations
    puredecl: ($) =>
      choice(
        seq(
          optional(choice("inline", "noinline")),
          "val",
          $.binder,
          "=",
          $.blockexpr,
        ),
        seq(
          optional(choice("inline", "noinline")),
          optional($.fipmod),
          "fun",
          $.funid,
          $.funbody,
        ),
      ),
    fipalloc: ($) => seq($._open_round_brace, choice($.int, "n"), ")"),
    fipmod: ($) =>
      choice(
        seq("fbip", optional($.fipalloc)),
        seq("fip", optional($.fipalloc)),
        "tail",
      ),
    fundecl: ($) => seq($.funid, $.funbody),
    binder: ($) => choice($.identifier, seq($.identifier, ":", $.type)),
    funid: ($) =>
      choice(
        $.identifier,
        seq($._open_square_brace, optional($.commas), "]"),
        $.string,
      ),
    funbody: ($) =>
      seq(
        optional($.typeparams),
        $._open_round_brace,
        optional($.pparameters),
        ")",
        choice($.bodyexpr, seq(":", $.tresult, optional($.qualifier), $.block)),
      ),
    annotres: ($) => seq(":", $.tresult),

    // Statements
    block: ($) =>
      seq($._open_brace_, optional($._semis), $.statements, $._close_brace_),
    statements: ($) => repeat1(seq($.statement, $._semis)),
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

    // Expressions
    bodyexpr: ($) => seq(optional("->"), $.blockexpr),
    blockexpr: ($) => $.expr,
    expr: ($) =>
      choice($.withexpr, $.block, $.returnexpr, $.valexpr, $.basicexpr),
    basicexpr: ($) =>
      choice($.ifexpr, $.matchexpr, $.handlerexpr, $.fnexpr, $.opexpr),
    matchexpr: ($) =>
      seq(
        "match",
        $.expr,
        $._open_brace_,
        optional($._semis),
        optional($.matchrules),
        $._close_brace_,
      ),
    fnexpr: ($) => seq("fn", $.funbody),
    returnexpr: ($) => seq("return", $.expr),
    ifexpr: ($) =>
      prec.right(
        seq(
          "if",
          $.expr,
          choice(
            seq("then", $.blockexpr, optional($.elifs)),
            seq("return", $.expr),
          ),
        ),
      ),
    elifs: ($) =>
      seq(
        repeat(seq("elif", $.expr, "then", $.blockexpr)),
        "else",
        $.blockexpr,
      ),
    valexpr: ($) => seq("val", $.apattern, "=", $.blockexpr, "in", $.expr),
    opexpr: ($) =>
      prec.right(seq($.prefixexpr, repeat(seq($.qoperator, $.prefixexpr)))),
    prefixexpr: ($) => choice(seq(choice("!", "~"), $.prefixexpr), $.appexpr),
    appexpr: ($) =>
      choice(
        seq(
          field("function", $.appexpr),
          choice(
            seq($._open_round_brace, optional($.arguments), ")"),
            seq($._open_square_brace, optional($.arguments), "]"),
            seq($.block),
            seq($.fnexpr),
          ),
        ),
        seq($.appexpr, ".", field("field", $.atom)),
        $.atom,
      ),
    atom: ($) =>
      choice(
        $.qidentifier,
        $.qconstructor,
        $.literal,
        $.mask,
        seq($._open_round_brace, optional($.aexprs), ")"),
        seq($._open_square_brace, optional($.cexprs), "]"),
      ),
    literal: ($) => choice($.int, $.float, $.char, $.string),
    mask: ($) =>
      seq(
        choice("mask", "inject"),
        optional(choice("behind", "other")),
        $._open_angle_brace,
        $.tbasic,
        ">",
      ),
    arguments: ($) => sep1($.argument, $._comma),
    argument: ($) => seq(optional(seq($.identifier, "=")), $.expr),
    parameters: ($) => sep1($.parameter, $._comma),
    parameter: ($) =>
      seq(
        optional("pub"),
        optional($.borrow),
        $.paramid,
        ":",
        $.type,
        optional(seq("=", $.expr)),
      ),
    paramid: ($) => choice($.identifier, $.wildcard),
    borrow: (_) => "^",
    pparameters: ($) => sep1($.pparameter, $._comma),
    pparameter: ($) =>
      seq(
        optional($.borrow),
        $.pattern,
        optional(seq(":", $.type)),
        optional(seq("=", $.expr)),
      ),
    aexprs: ($) => sep1($.aexpr, $._comma),
    cexprs: ($) => seq(sep1($.aexpr, $._comma), optional($._comma)),
    aexpr: ($) => seq($.expr, optional($.annot)),
    annot: ($) => seq(":", $.typescheme),

    // Identifiers and operators
    qoperator: ($) => $.op,
    qidentifier: ($) => choice($.qvarid, $.qidop, $.identifier),
    identifier: ($) => choice($.varid, $.idop),
    qvarid: ($) => $.qid,
    varid: ($) => $.id,
    qconstructor: ($) => choice($.conid, $.qconid),

    // Matching
    matchrules: ($) => repeat1(seq($.matchrule, $._semis)),
    matchrule: ($) =>
      seq($.patterns, optional(seq("|", $.expr)), "->", $.blockexpr),
    patterns: ($) => sep1($.pattern, $._comma),
    apatterns: ($) => sep1($.apattern, $._comma),
    apattern: ($) => seq($.pattern, optional($.annot)),
    pattern: ($) =>
      choice(
        seq($.identifier, optional(seq("as", $.pattern))),
        seq(
          $.qconstructor,
          optional(seq($._open_round_brace, optional($.patargs), ")")),
        ),
        seq($._open_round_brace, optional($.apatterns), ")"),
        seq($._open_square_brace, optional($.apatterns), "]"),
        $.literal,
        $.wildcard,
      ),
    patargs: ($) => sep1($.patarg, $._comma),
    patarg: ($) => seq(optional(seq($.identifier, "=")), $.apattern),

    // Handlers
    handlerexpr: ($) =>
      choice(
        seq(
          optional(choice("override", "named")),
          "handler",
          optional($.witheff),
          $.opclauses,
        ),
        seq(
          optional(choice("override", "named")),
          "handle",
          optional($.witheff),
          $.expr,
          $.opclauses,
        ),
      ),
    witheff: ($) => seq($._open_angle_brace, $.anntype, ">"),
    withstat: ($) =>
      choice(
        seq("with", $.basicexpr),
        seq(
          "with",
          $.binder,
          "<-",
          choice($.basicexpr, seq(optional($.witheff), $.opclause)),
        ),
        seq("with", optional("override"), optional($.witheff), $.opclause),
      ),
    withexpr: ($) => seq($.withstat, "in", $.blockexpr),
    opclauses: ($) =>
      seq(
        $._open_brace_,
        optional($._semis),
        optional(
          seq($.opclausex, repeat(seq($._semis, $.opclausex)), $._semis),
        ),
        $._close_brace_,
      ),
    opclausex: ($) =>
      choice(
        seq("finally", $.bodyexpr),
        seq("initially", $._open_round_brace, $.opparam, ")", $.bodyexpr),
        $.opclause,
      ),
    opclause: ($) =>
      choice(
        seq("val", $.qidentifier, "=", $.blockexpr),
        seq("val", $.qidentifier, ":", $.type, "=", $.blockexpr),
        seq("fun", $.qidentifier, $.opparams, $.bodyexpr),
        seq(
          choice(
            "control",
            "rcontrol",
            "rawctl",
            seq(optional($.controlmod), "ctl"),
          ),
          $.qidentifier,
          $.opparams,
          $.bodyexpr,
        ),
        seq("return", $._open_round_brace, $.opparam, ")", $.bodyexpr),
      ),
    controlmod: (_) => choice("final", "raw"),
    opparams: ($) =>
      seq($._open_round_brace, optional(sep1($.opparam, $._comma)), ")"),
    opparam: ($) => seq($.paramid, optional(seq(":", $.type))),

    // Types
    tbinders: ($) => sep1($.tbinder, $._comma),
    tbinder: ($) => seq($.varid, optional($.kannot)),
    typescheme: ($) =>
      seq(optional($.someforalls), $.tarrow, optional($.qualifier)),
    type: ($) =>
      seq(
        optional(seq("forall", $.typeparams)),
        $.tarrow,
        optional($.qualifier),
      ),
    someforalls: ($) =>
      choice(
        seq("some", $.typeparams, optional(seq("forall", $.typeparams))),
        seq("forall", $.typeparams),
      ),
    typeparams: ($) => seq($._open_angle_brace, optional($.tbinders), ">"),
    qualifier: ($) => seq("with", $._open_round_brace, $.predicates, ")"),
    predicates: ($) => sep1($.predicate, $._comma),
    predicate: ($) => $.typeapp,
    tarrow: ($) => seq($.tatomic, optional(seq("->", $.tresult))),
    tresult: ($) => seq($.tatomic, optional($.tbasic)),
    tatomic: ($) =>
      choice(
        $.tbasic,
        seq(
          $._open_angle_brace,
          optional(seq($.targuments, optional(seq("|", $.tatomic)))),
          ">",
        ),
      ),
    tbasic: ($) =>
      choice(
        $.typeapp,
        seq($._open_round_brace, optional($.tparams), ")"),
        seq($._open_square_brace, $.anntype, "]"),
      ),
    typeapp: ($) =>
      seq(
        $.typecon,
        optional(seq($._open_angle_brace, optional($.targuments), ">")),
      ),
    typecon: ($) =>
      choice(
        $.varid,
        $.qvarid,
        $.wildcard,
        seq($._open_round_brace, $.commas, ")"),
        seq($._open_square_brace, "]"),
        seq($._open_round_brace, "->", ")"),
      ),
    tparams: ($) => sep1($.tparam, $._comma),
    tparam: ($) => seq(optional(seq($.identifier, ":")), $.anntype),
    targuments: ($) => sep1($.anntype, $._comma),
    anntype: ($) => seq($.type, optional($.kannot)),

    // Kinds
    kannot: ($) => seq("::", $.kind),
    kind: ($) =>
      choice(
        seq($._open_round_brace, $.kinds, ")", "->", $.katom),
        seq($.katom, optional(seq("->", $.kind))),
      ),
    kinds: ($) => sep1($.kind, $._comma),
    katom: ($) => $.conid,

    // Character classes
    _symbols: (_) => /[$%&*+@!\\^~=.\-:?|<>]+|\//,
    conid: (_) => /[A-Z][a-zA-Z0-9_-]*'*/,
    id: (_) => /[a-z][a-zA-Z0-9_-]*'*/,
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
    qconid: (_) => /([a-z][a-zA-Z0-9_-]*'*\/)+[A-Z][a-zA-Z0-9_-]*'*/,
    qid: (_) => /([a-z][a-zA-Z0-9_-]*'*\/)+[a-z][a-zA-Z0-9_-]*'*/,
    qidop: (_) => /([a-z][a-zA-Z0-9_-]*'*\/)+\(([$%&*+@!\\^~=.\-:?|<>]+|\/)\)/,
    idop: (_) =>
      seq(
        "(",
        token.immediate(/[$%&*+@!\\^~=.\-:?|<>]+|\//),
        token.immediate(")"),
      ),
    op: ($) =>
      prec.right(seq($._symbols, optional($._end_continuation_signal))),
    wildcard: (_) => /_[a-zA-Z0-9_-]*/,

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
 * Creates a rule to match one or more of the rules separated by a comma
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
