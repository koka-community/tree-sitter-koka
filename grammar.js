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
  rules: {
    program: ($) =>
      choice(seq($.semis, $.MODULE, $.modulepath, $.moduledecl), $.moduledecl),
    moduledecl: ($) =>
      choice(
        seq("{", $.semis, $.imports, "}", $.semis),
        seq($.semis, $.imports),
      ),
    imports: ($) => seq(repeat(seq($.importdecl, $.semis1)), $.eimports),
    eimports: ($) =>
      seq(
        repeat(seq($.IMPORT_EXTERN, $.externimpbody, $.semis1)),
        $.declarations,
      ),
    importdecl: ($) =>
      choice(
        seq($.pub, $.IMPORT, $.modulepath),
        seq($.pub, $.IMPORT, $.modulepath, "=", $.modulepath),
      ),
    modulepath: ($) => choice($.varid, $.qvarid),
    pub: ($) => optional($.PUB),
    semis1: ($) => repeat1($.semi),
    semis: ($) => repeat($.semi),
    semi: ($) => choice(";", $._semi),
    declarations: ($) => seq(repeat(seq($.fixitydecl, $.semis1)), $.topdecls),
    fixitydecl: ($) => seq($.pub, $.fixity, $.oplist1),
    fixity: ($) =>
      choice(seq($.INFIX, $.INT), seq($.INFIXR, $.INT), seq($.INFIXL, $.INT)),
    oplist1: ($) => seq($.identifier, repeat(seq(",", $.identifier))),
    topdecls: ($) => optional($.topdecls1),
    topdecls1: ($) =>
      repeat1(choice(seq($.topdecl, $.semis1), seq($.error, $.semis1))),
    topdecl: ($) =>
      choice(
        seq($.pub, $.puredecl),
        seq($.pub, $.aliasdecl),
        seq($.pub, $.externdecl),
        seq($.pub, $.typedecl),
        seq($.ABSTRACT, $.typedecl),
      ),
    externdecl: ($) =>
      seq(
        $.inlinemod,
        $.fipmod,
        $.EXTERN,
        $.qidentifier,
        $.externtype,
        $.externbody,
      ),
    externtype: ($) =>
      choice(
        seq(":", $.typescheme),
        seq($.typeparams, "(", $.parameters, ")", $.annotres),
      ),
    externbody: ($) =>
      choice(seq("{", $.semis, $.externstats1, "}"), seq("{", $.semis, "}")),
    externstats1: ($) =>
      seq($.externstat, $.semis1, repeat(seq($.externstat, $.semis1))),
    externstat: ($) =>
      choice(
        seq($.externtarget, $.externinline, $.STRING),
        seq($.externinline, $.STRING),
      ),
    externinline: ($) => optional($.ID_INLINE),
    externimpbody: ($) =>
      choice(seq("=", $.externimp), seq("{", $.semis, $.externimps1, "}")),
    externimps1: ($) =>
      seq($.externimp, $.semis1, repeat(seq($.externimp, $.semis1))),
    externimp: ($) =>
      choice(
        seq($.externtarget, $.varid, $.STRING),
        seq($.externtarget, "{", $.externvals1, "}"),
      ),
    externvals1: ($) =>
      seq($.externval, $.semis1, repeat(seq($.externval, $.semis1))),
    externval: ($) => seq($.varid, "=", $.STRING),
    externtarget: ($) => choice($.ID_CS, $.ID_JS, $.ID_C),
    aliasdecl: ($) =>
      seq($.ALIAS, $.typeid, $.typeparams, $.kannot, "=", $.type),
    typedecl: ($) =>
      choice(
        seq($.typemod, $.TYPE, $.typeid, $.typeparams, $.kannot, $.typebody),
        seq(
          $.structmod,
          $.STRUCT,
          $.typeid,
          $.typeparams,
          $.kannot,
          $.conparams,
        ),
        seq($.effectmod, $.EFFECT, $.varid, $.typeparams, $.kannot, $.opdecls),
        seq($.effectmod, $.EFFECT, $.typeparams, $.kannot, $.operation),
        seq(
          $.NAMED,
          $.effectmod,
          $.EFFECT,
          $.varid,
          $.typeparams,
          $.kannot,
          $.opdecls,
        ),
        seq(
          $.NAMED,
          $.effectmod,
          $.EFFECT,
          $.typeparams,
          $.kannot,
          $.operation,
        ),
        seq(
          $.NAMED,
          $.effectmod,
          $.EFFECT,
          $.varid,
          $.typeparams,
          $.kannot,
          $.IN,
          $.type,
          $.opdecls,
        ),
      ),
    typemod: ($) =>
      choice($.structmod, $.ID_OPEN, $.ID_EXTEND, $.ID_CO, $.ID_DIV, $.ID_LAZY),
    structmod: ($) => optional(choice($.ID_VALUE, $.ID_REFERENCE)),
    effectmod: ($) =>
      optional(choice($.ID_DIV, $.ID_LINEAR, seq($.ID_LINEAR, $.ID_DIV))),
    typebody: ($) => optional(seq("{", $.semis, $.constructors, "}")),
    typeid: ($) =>
      choice(
        seq("(", $.commas, ")"),
        seq("[", "]"),
        seq("<", ">"),
        seq("<", "|", ">"),
        $.varid,
        $.CTX,
      ),
    commas: ($) => optional($.commas1),
    commas1: ($) => seq($.commas, ","),
    constructors: ($) => optional(seq($.constructors1, $.semis1)),
    constructors1: ($) =>
      seq($.constructor, repeat(seq($.semis1, $.constructor))),
    constructor: ($) =>
      choice(
        seq($.pub, $.con, $.conid, $.typeparams, $.conparams),
        seq(
          $.pub,
          $.ID_LAZY,
          $.con,
          $.conid,
          $.typeparams,
          $.conparams,
          $.RARROW,
          $.blockexpr,
        ),
      ),
    con: ($) => optional($.CON),
    conparams: ($) =>
      optional(
        choice(
          seq("(", $.parameters1, ")"),
          seq("{", $.semis, $.sconparams, "}"),
        ),
      ),
    sconparams: ($) => repeat(seq($.parameter, $.semis1)),
    opdecls: ($) => seq("{", $.semis, $.operations, "}"),
    operations: ($) => repeat(seq($.operation, $.semis1)),
    operation: ($) =>
      choice(
        seq($.pub, $.VAL, $.identifier, $.typeparams, ":", $.tatomic),
        seq(
          $.pub,
          $.FUN,
          $.identifier,
          $.typeparams,
          "(",
          $.pparameters,
          ")",
          ":",
          $.tatomic,
        ),
        seq(
          $.pub,
          $.controlmod,
          $.CTL,
          $.identifier,
          $.typeparams,
          "(",
          $.pparameters,
          ")",
          ":",
          $.tatomic,
        ),
      ),
    puredecl: ($) =>
      choice(
        seq($.inlinemod, $.VAL, $.binder, "=", $.blockexpr),
        seq($.inlinemod, $.fipmod, $.FUN, $.qidentifier, $.funbody),
      ),
    inlinemod: ($) => optional(choice($.ID_INLINE, $.ID_NOINLINE)),
    fipmod: ($) =>
      choice(
        seq($.tailmod, $.ID_FIP, $.fiplimit),
        seq($.tailmod, $.ID_FBIP, $.fiplimit),
        $.tailmod,
      ),
    fiplimit: ($) => optional(choice(seq("(", $.INT, ")"), seq("(", "_", ")"))),
    tailmod: ($) => optional($.ID_TAIL),
    fundecl: ($) => seq($.identifier, $.funbody),
    binder: ($) => choice($.identifier, seq($.identifier, ":", $.type)),
    funbody: ($) =>
      choice(
        seq($.typeparams, "(", $.pparameters, ")", $.bodyexpr),
        seq(
          $.typeparams,
          "(",
          $.pparameters,
          ")",
          ":",
          $.tresult,
          $.qualifier,
          $.block,
        ),
      ),
    annotres: ($) => optional(seq(":", $.tresult)),
    block: ($) => seq("{", $.semis, $.statements1, "}"),
    statements1: ($) =>
      seq(
        choice(seq($.statement, $.semis1), seq($.error, $.semis1)),
        repeat(seq($.statement, $.semis1)),
      ),
    statement: ($) =>
      choice(
        $.decl,
        $.withstat,
        seq($.withstat, $.IN, $.blockexpr),
        $.returnexpr,
        $.basicexpr,
      ),
    decl: ($) =>
      choice(
        seq($.FUN, $.fundecl),
        seq($.VAL, $.apattern, "=", $.blockexpr),
        seq($.VAR, $.binder, $.ASSIGN, $.blockexpr),
      ),
    bodyexpr: ($) => choice($.blockexpr, seq($.RARROW, $.blockexpr)),
    blockexpr: ($) => $.expr,
    expr: ($) =>
      choice($.withexpr, $.block, $.returnexpr, $.valexpr, $.basicexpr),
    basicexpr: ($) =>
      choice($.ifexpr, $.matchexpr, $.handlerexpr, $.fnexpr, $.opexpr),
    matchexpr: ($) =>
      choice(
        seq($.MATCH, $.ntlexpr, "{", $.semis, $.matchrules, "}"),
        seq($.ID_LAZY, $.MATCH, $.ntlexpr, "{", $.semis, $.matchrules, "}"),
      ),
    fnexpr: ($) => seq($.FN, $.funbody),
    returnexpr: ($) => seq($.RETURN, $.expr),
    ifexpr: ($) =>
      choice(
        seq($.IF, $.ntlexpr, $.THEN, $.blockexpr, $.elifs),
        seq($.IF, $.ntlexpr, $.THEN, $.blockexpr),
        seq($.IF, $.ntlexpr, $.RETURN, $.expr),
      ),
    elifs: ($) =>
      seq(
        repeat(seq($.ELIF, $.ntlexpr, $.THEN, $.blockexpr)),
        $.ELSE,
        $.blockexpr,
      ),
    valexpr: ($) => seq($.VAL, $.apattern, "=", $.blockexpr, $.IN, $.expr),
    opexpr: ($) => seq($.prefixexpr, repeat(seq($.qoperator, $.prefixexpr))),
    prefixexpr: ($) => seq(repeat(choice("!", "~")), $.appexpr),
    appexpr: ($) =>
      seq(
        $.atom,
        repeat(
          choice(
            seq("(", $.arguments, ")"),
            seq("[", $.arguments, "]"),
            seq(".", $.name),
            seq(".", "(", $.arguments, ")"),
            $.block,
            $.fnexpr,
          ),
        ),
      ),
    ntlexpr: ($) => $.ntlopexpr,
    ntlopexpr: ($) =>
      seq($.ntlprefixexpr, repeat(seq($.qoperator, $.ntlprefixexpr))),
    ntlprefixexpr: ($) => seq(repeat(choice("!", "~")), $.ntlappexpr),
    ntlappexpr: ($) =>
      seq(
        $.atom,
        repeat(
          choice(
            seq("(", $.arguments, ")"),
            seq("[", $.arguments, "]"),
            seq(".", $.name),
            seq(".", "(", $.arguments, ")"),
          ),
        ),
      ),
    atom: ($) =>
      choice(
        $.name,
        $.literal,
        $.mask,
        seq("(", $.aexprs, ")"),
        seq("[", $.cexprs, "]"),
        $.ctxexpr,
        $.ctxhole,
      ),
    name: ($) => choice($.qidentifier, $.qconstructor, $.qimplicit),
    literal: ($) => choice($.INT, $.FLOAT, $.CHAR, $.STRING),
    mask: ($) => seq($.MASK, $.behind, "<", $.tbasic, ">"),
    behind: ($) => optional($.ID_BEHIND),
    ctxexpr: ($) => seq($.CTX, $.atom),
    ctxhole: ($) => "_",
    arguments: ($) => optional($.arguments1),
    arguments1: ($) => seq($.argument, repeat(seq(",", $.argument))),
    argument: ($) =>
      choice(
        $.expr,
        seq($.identifier, "=", $.expr),
        seq($.qimplicit, "=", $.expr),
      ),
    parameters: ($) => optional($.parameters1),
    parameters1: ($) => seq($.parameter, repeat(seq(",", $.parameter))),
    parameter: ($) =>
      choice(
        seq($.borrow, $.paramid, ":", $.type),
        seq($.borrow, $.paramid, ":", $.type, "=", $.expr),
      ),
    paramid: ($) => choice($.identifier, $.wildcard),
    borrow: ($) => optional("^"),
    pparameters: ($) => optional($.pparameters1),
    pparameters1: ($) => seq($.pparameter, repeat(seq(",", $.pparameter))),
    pparameter: ($) =>
      choice(
        seq($.borrow, $.pattern),
        seq($.borrow, $.pattern, ":", $.type),
        seq($.borrow, $.pattern, ":", $.type, "=", $.expr),
        seq($.borrow, $.pattern, "=", $.expr),
        seq($.borrow, $.qimplicit),
        seq($.borrow, $.qimplicit, ":", $.type),
      ),
    aexprs: ($) => optional($.aexprs1),
    aexprs1: ($) => seq($.aexpr, repeat(seq(",", $.aexpr))),
    cexprs: ($) => choice($.cexprs0, seq($.cexprs0, $.aexpr)),
    cexprs0: ($) => repeat(seq($.aexpr, ",")),
    aexpr: ($) => seq($.expr, $.annot),
    annot: ($) => optional(seq(":", $.typescheme)),
    qoperator: ($) => $.op,
    qidentifier: ($) => choice($.qvarid, $.QIDOP, $.identifier),
    identifier: ($) => choice($.varid, $.IDOP),
    wildcard: ($) => choice($.WILDCARDID, "_"),
    //qimplicit: ($) => $.IMPLICITID,
    qvarid: ($) => $.QID,
    varid: ($) =>
      choice(
        $.ID,
        $.ID_C,
        $.ID_CS,
        $.ID_JS,
        $.ID_FILE,
        $.ID_INLINE,
        $.ID_NOINLINE,
        $.ID_OPEN,
        $.ID_EXTEND,
        $.ID_LINEAR,
        $.ID_BEHIND,
        $.ID_VALUE,
        $.ID_REFERENCE,
        $.ID_SCOPED,
        $.ID_INITIALLY,
        $.ID_FINALLY,
        $.ID_DIV,
        $.ID_CO,
        $.ID_FIP,
        $.ID_FBIP,
        $.ID_TAIL,
        $.ID_LAZY,
      ),
    qconstructor: ($) => choice($.conid, $.qconid),
    qconid: ($) => $.QCONID,
    conid: ($) => $.CONID,
    op: ($) => choice($.OP, ">", "<", "|", $.ASSIGN),
    matchrules: ($) => optional(seq($.matchrules1, $.semis1)),
    matchrules1: ($) => seq($.matchrule, repeat(seq($.semis1, $.matchrule))),
    matchrule: ($) =>
      choice(
        seq($.patterns1, "|", $.expr, $.RARROW, $.blockexpr),
        seq($.patterns1, $.RARROW, $.blockexpr),
      ),
    patterns1: ($) => seq($.pattern, repeat(seq(",", $.pattern))),
    apatterns: ($) => optional($.apatterns1),
    apatterns1: ($) => seq($.apattern, repeat(seq(",", $.apattern))),
    apattern: ($) => seq($.pattern, $.annot),
    pattern: ($) =>
      seq(
        repeat(seq($.identifier, $.AS)),
        choice(
          $.identifier,
          $.conid,
          seq($.conid, "(", $.patargs, ")"),
          seq("(", $.apatterns, ")"),
          seq("[", $.apatterns, "]"),
          $.literal,
          $.wildcard,
        ),
      ),
    patargs: ($) => optional($.patargs1),
    patargs1: ($) => choice(seq($.patargs, ",", $.patarg), $.patarg),
    patarg: ($) => choice(seq($.identifier, "=", $.apattern), $.apattern),
    handlerexpr: ($) =>
      choice(
        seq($.override, $.HANDLER, $.witheff, $.opclauses),
        seq($.override, $.HANDLE, $.witheff, $.ntlexpr, $.opclauses),
        seq($.NAMED, $.HANDLER, $.witheff, $.opclauses),
        seq($.NAMED, $.HANDLE, $.witheff, $.ntlexpr, $.opclauses),
      ),
    override: ($) => optional($.OVERRIDE),
    witheff: ($) => optional(seq("<", $.anntype, ">")),
    withstat: ($) =>
      choice(
        seq($.WITH, $.basicexpr),
        seq($.WITH, $.binder, $.LARROW, $.basicexpr),
        seq($.WITH, $.override, $.witheff, $.opclause),
        seq($.WITH, $.binder, $.LARROW, $.witheff, $.opclause),
      ),
    withexpr: ($) => seq($.withstat, $.IN, $.blockexpr),
    opclauses: ($) =>
      choice(
        seq("{", $.semis, $.opclauses1, $.semis1, "}"),
        seq("{", $.semis, "}"),
      ),
    opclauses1: ($) => seq($.opclausex, repeat(seq($.semis1, $.opclausex))),
    opclausex: ($) =>
      choice(
        seq($.ID_FINALLY, $.bodyexpr),
        seq($.ID_INITIALLY, "(", $.opparam, ")", $.bodyexpr),
        $.opclause,
      ),
    opclause: ($) =>
      choice(
        seq($.VAL, $.qidentifier, "=", $.blockexpr),
        seq($.VAL, $.qidentifier, ":", $.type, "=", $.blockexpr),
        seq($.FUN, $.qidentifier, $.opparams, $.bodyexpr),
        seq($.controlmod, $.CTL, $.qidentifier, $.opparams, $.bodyexpr),
        seq($.RETURN, "(", $.opparam, ")", $.bodyexpr),
      ),
    controlmod: ($) => optional(choice($.FINAL, $.RAW)),
    opparams: ($) => seq("(", $.opparams0, ")"),
    opparams0: ($) => optional($.opparams1),
    opparams1: ($) => seq($.opparam, repeat(seq(",", $.opparam))),
    opparam: ($) => choice($.paramid, seq($.paramid, ":", $.type)),
    tbinders: ($) => optional($.tbinders1),
    tbinders1: ($) => seq($.tbinder, repeat(seq(",", $.tbinder))),
    tbinder: ($) => seq($.varid, $.kannot),
    typescheme: ($) => seq($.someforalls, $.tarrow, $.qualifier),
    type: ($) =>
      choice(
        seq($.FORALL, $.typeparams1, $.tarrow, $.qualifier),
        seq($.tarrow, $.qualifier),
      ),
    someforalls: ($) =>
      optional(
        choice(
          seq($.SOME, $.typeparams1, $.FORALL, $.typeparams1),
          seq($.SOME, $.typeparams1),
          seq($.FORALL, $.typeparams1),
        ),
      ),
    typeparams: ($) => optional($.typeparams1),
    typeparams1: ($) => seq("<", $.tbinders, ">"),
    qualifier: ($) => optional(seq($.WITH, "(", $.predicates1, ")")),
    predicates1: ($) => seq($.predicate, repeat(seq(",", $.predicate))),
    predicate: ($) => $.typeapp,
    tarrow: ($) => choice(seq($.tatomic, $.RARROW, $.tresult), $.tatomic),
    tresult: ($) => choice(seq($.tatomic, $.tbasic), $.tatomic),
    tatomic: ($) =>
      choice(
        $.tbasic,
        seq("<", $.targuments1, "|", $.tatomic, ">"),
        seq("<", $.targuments, ">"),
      ),
    tbasic: ($) =>
      choice($.typeapp, seq("(", $.tparams, ")"), seq("[", $.anntype, "]")),
    typeapp: ($) => choice($.typecon, seq($.typecon, "<", $.targuments, ">")),
    typecon: ($) =>
      choice(
        $.varid,
        $.qvarid,
        $.wildcard,
        seq("(", $.commas1, ")"),
        seq("[", "]"),
        seq("(", $.RARROW, ")"),
        $.CTX,
      ),
    tparams: ($) => optional($.tparams1),
    tparams1: ($) => seq($.tparam, repeat(seq(",", $.tparam))),
    tparam: ($) => choice(seq($.identifier, ":", $.anntype), $.anntype),
    targuments: ($) => optional($.targuments1),
    targuments1: ($) => seq($.anntype, repeat(seq(",", $.anntype))),
    anntype: ($) => seq($.type, $.kannot),
    kannot: ($) => optional(seq($.DCOLON, $.kind)),
    kind: ($) =>
      seq(
        repeat(seq($.katom, $.RARROW)),
        choice(seq("(", $.kinds1, ")", $.RARROW, $.katom), $.katom),
      ),
    kinds1: ($) => seq($.kind, repeat(seq(",", $.kind))),
    katom: ($) => $.conid,
    // =================
    // CUSTOM DEFINITIONS
    // =================
    INFIX: ($) => "infix",
    INFIXL: ($) => "infixl",
    INFIXR: ($) => "infixr",
    TYPE: ($) => "type",
    ALIAS: ($) => "alias",
    STRUCT: ($) => "struct",
    EFFECT: ($) => "effect",
    FORALL: ($) => "forall",
    EXISTS: ($) => "exists",
    SOME: ($) => "some",
    ABSTRACT: ($) => "abstract",
    EXTERN: ($) => "extern",
    FUN: ($) => "fun",
    FN: ($) => "fn",
    VAL: ($) => "val",
    VAR: ($) => "var",
    CON: ($) => "con",
    IF: ($) => "if",
    THEN: ($) => "then",
    ELSE: ($) => "else",
    ELIF: ($) => "elif",
    WITH: ($) => "with",
    IN: ($) => "in",
    MATCH: ($) => "match",
    RETURN: ($) => "return",
    CTX: ($) => "ctx",
    MODULE: ($) => "module",
    IMPORT: ($) => "import",
    PUB: ($) => "pub",
    AS: ($) => "as",
    HANDLE: ($) => "handle",
    HANDLER: ($) => "handler",
    CTL: ($) => "ctl",
    FINAL: ($) => "final",
    RAW: ($) => "raw",
    MASK: ($) => "mask",
    OVERRIDE: ($) => "override",
    NAMED: ($) => "named",
    ID_DIV: ($) => "div",
    ID_CO: ($) => "co",
    ID_OPEN: ($) => "open",
    ID_EXTEND: ($) => "extend",
    ID_LINEAR: ($) => "linear",
    ID_VALUE: ($) => "value",
    ID_REFERENCE: ($) => "reference",
    ID_INLINE: ($) => "inline",
    ID_NOINLINE: ($) => "noinline",
    ID_SCOPED: ($) => "scoped",
    ID_BEHIND: ($) => "behind",
    ID_INITIALLY: ($) => "initially",
    ID_FINALLY: ($) => "finally",
    ID_FIP: ($) => "fip",
    ID_FBIP: ($) => "fbip",
    ID_TAIL: ($) => "tail",
    ID_LAZY: ($) => "lazy",
    IMPORT_EXTERN: ($) => "import",
    IFACE: ($) => "interface",
    BREAK: ($) => "break",
    CONTINUE: ($) => "continue",
    UNSAFE: ($) => "unsafe",
    RARROW: ($) => "\-\>",
    LARROW: ($) => "\<\-",
    ASSIGN: ($) => ":=",
    DCOLON: ($) => "::",
    ID_FILE: ($) => "file",
    ID_CS: ($) => "cs",
    ID_JS: ($) => "js",
    ID_C: ($) => "c",
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
    FLOAT: (_) =>
      choice(
        /-?(0|[1-9](_?[0-9]+(_[0-9]+)*)?)((\.[0-9]+(_[0-9]+)*)?[eE][-+]?[0-9]+|\.[0-9]+(_[0-9]+)*)/,
        /-?0[xX][0-9a-fA-F]+(_[0-9a-fA-F]+)*((\.[0-9a-fA-F]+(_[0-9a-fA-F]+)*)?[pP][-+]?[0-9]+|\.[0-9a-fA-F]+(_[0-9a-fA-F]+)*)/,
      ),
    INT: (_) =>
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
    OP: ($) =>
      prec.right(seq($._symbols, optional($._end_continuation_signal))),
    WILDCARDID: (_) => /_[a-zA-Z0-9_-]*/,
    qimplicit: ($) => seq("?", choice($.QID, $.QIDOP)),

    // String
    STRING: ($) =>
      choice(
        $._raw_string,
        seq(
          '"',
          repeat(choice(token.immediate(/[^\\"]+/), $.escape)),
          token.immediate('"'),
        ),
      ),
    CHAR: ($) =>
      seq(
        "'",
        choice(token.immediate(/[^\\']/), $.escape),
        token.immediate("'"),
      ),

  // TODO: manage error
    error: ($) => seq(),

  },
});
