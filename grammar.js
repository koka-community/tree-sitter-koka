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
      choice(
        seq(optional($.semis), $.MODULE, $.modulepath, optional($.moduledecl)),
        $.moduledecl,
      ),
    moduledecl: ($) =>
      choice(
        seq(
          "{",
          optional($.semis),
          optional($.imports),
          "}",
          optional($.semis),
        ),
        seq($.semis, optional($.imports)),
      ),
    imports: ($) =>
      seq(repeat1(seq($.importdecl, $.semis1)), optional($.eimports)),
    eimports: ($) =>
      seq(
        repeat1(seq($.IMPORT_EXTERN, $.externimpbody, $.semis1)),
        optional($.declarations),
      ),
    importdecl: ($) =>
      choice(
        seq(optional($.pub), $.IMPORT, $.modulepath),
        seq(optional($.pub), $.IMPORT, $.modulepath, "=", $.modulepath),
      ),
    modulepath: ($) => choice($.varid, $.qvarid),
    pub: ($) => $.PUB,
    semis1: ($) => repeat1($.semi),
    semis: ($) => repeat1($.semi),
    semi: ($) => $._semi,
    declarations: ($) =>
      seq(repeat1(seq($.fixitydecl, $.semis1)), optional($.topdecls)),
    fixitydecl: ($) => seq(optional($.pub), $.fixity, $.oplist1),
    fixity: ($) =>
      choice(seq($.INFIX, $.INT), seq($.INFIXR, $.INT), seq($.INFIXL, $.INT)),
    oplist1: ($) => seq($.identifier, repeat(seq(",", $.identifier))),
    topdecls: ($) => $.topdecls1,
    topdecls1: ($) =>
      repeat1(choice(seq($.topdecl, $.semis1), seq($.error, $.semis1))),
    topdecl: ($) =>
      choice(
        seq(optional($.pub), $.puredecl),
        seq(optional($.pub), $.aliasdecl),
        seq(optional($.pub), $.externdecl),
        seq(optional($.pub), $.typedecl),
        seq($.ABSTRACT, $.typedecl),
      ),
    externdecl: ($) =>
      seq(
        optional($.inlinemod),
        optional($.fipmod),
        $.EXTERN,
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
        seq("{", optional($.semis), $.externstats1, "}"),
        seq("{", optional($.semis), "}"),
      ),
    externstats1: ($) =>
      seq($.externstat, $.semis1, repeat(seq($.externstat, $.semis1))),
    externstat: ($) =>
      choice(
        seq($.externtarget, optional($.externinline), $.STRING),
        seq(optional($.externinline), $.STRING),
      ),
    externinline: ($) => $.ID_INLINE,
    externimpbody: ($) =>
      choice(
        seq("=", $.externimp),
        seq("{", optional($.semis), $.externimps1, "}"),
      ),
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
      seq(
        $.ALIAS,
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
          $.TYPE,
          $.typeid,
          optional($.typeparams),
          optional($.kannot),
          optional($.typebody),
        ),
        seq(
          optional($.structmod),
          $.STRUCT,
          $.typeid,
          optional($.typeparams),
          optional($.kannot),
          optional($.conparams),
        ),
        seq(
          optional($.effectmod),
          $.EFFECT,
          $.varid,
          optional($.typeparams),
          optional($.kannot),
          $.opdecls,
        ),
        seq(
          optional($.effectmod),
          $.EFFECT,
          optional($.typeparams),
          optional($.kannot),
          $.operation,
        ),
        seq(
          $.NAMED,
          optional($.effectmod),
          $.EFFECT,
          $.varid,
          optional($.typeparams),
          optional($.kannot),
          $.opdecls,
        ),
        seq(
          $.NAMED,
          optional($.effectmod),
          $.EFFECT,
          optional($.typeparams),
          optional($.kannot),
          $.operation,
        ),
        seq(
          $.NAMED,
          optional($.effectmod),
          $.EFFECT,
          $.varid,
          optional($.typeparams),
          optional($.kannot),
          $.IN,
          $.type,
          $.opdecls,
        ),
      ),
    typemod: ($) =>
      choice($.structmod, $.ID_OPEN, $.ID_EXTEND, $.ID_CO, $.ID_DIV, $.ID_LAZY),
    structmod: ($) => choice($.ID_VALUE, $.ID_REFERENCE),
    effectmod: ($) => choice($.ID_DIV, $.ID_LINEAR, seq($.ID_LINEAR, $.ID_DIV)),
    typebody: ($) => seq("{", optional($.semis), optional($.constructors), "}"),
    typeid: ($) =>
      choice(
        seq("(", optional($.commas), ")"),
        seq("[", "]"),
        seq("<", ">"),
        seq("<", "|", ">"),
        $.varid,
        $.CTX,
      ),
    commas: ($) => $.commas1,
    commas1: ($) => seq(optional($.commas), ","),
    constructors: ($) => seq($.constructors1, $.semis1),
    constructors1: ($) =>
      prec.left(seq($.constructor, repeat(seq($.semis1, $.constructor)))),
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
          $.ID_LAZY,
          optional($.con),
          $.conid,
          optional($.typeparams),
          optional($.conparams),
          $.RARROW,
          $.blockexpr,
        ),
      ),
    con: ($) => $.CON,
    conparams: ($) =>
      choice(
        seq("(", $.parameters1, ")"),
        seq("{", optional($.semis), optional($.sconparams), "}"),
      ),
    sconparams: ($) => repeat1(seq($.parameter, $.semis1)),
    opdecls: ($) => seq("{", optional($.semis), optional($.operations), "}"),
    operations: ($) => repeat1(seq($.operation, $.semis1)),
    operation: ($) =>
      choice(
        seq(
          optional($.pub),
          $.VAL,
          $.identifier,
          optional($.typeparams),
          ":",
          $.tatomic,
        ),
        seq(
          optional($.pub),
          $.FUN,
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
          $.CTL,
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
        seq(optional($.inlinemod), $.VAL, $.binder, "=", $.blockexpr),
        seq(
          optional($.inlinemod),
          optional($.fipmod),
          $.FUN,
          $.qidentifier,
          $.funbody,
        ),
      ),
    inlinemod: ($) => choice($.ID_INLINE, $.ID_NOINLINE),
    fipmod: ($) =>
      choice(
        seq(optional($.tailmod), $.ID_FIP, optional($.fiplimit)),
        seq(optional($.tailmod), $.ID_FBIP, optional($.fiplimit)),
        $.tailmod,
      ),
    fiplimit: ($) => choice(seq("(", $.INT, ")"), seq("(", "_", ")")),
    tailmod: ($) => $.ID_TAIL,
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
    block: ($) => seq("{", optional($.semis), $.statements1, "}"),
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
        seq(
          $.MATCH,
          $.ntlexpr,
          "{",
          optional($.semis),
          optional($.matchrules),
          "}",
        ),
        seq(
          $.ID_LAZY,
          $.MATCH,
          $.ntlexpr,
          "{",
          optional($.semis),
          optional($.matchrules),
          "}",
        ),
      ),
    fnexpr: ($) => seq($.FN, $.funbody),
    returnexpr: ($) => seq($.RETURN, $.expr),
    ifexpr: ($) =>
      prec.left(
        choice(
          seq($.IF, $.ntlexpr, $.THEN, $.blockexpr, $.elifs),
          seq($.IF, $.ntlexpr, $.THEN, $.blockexpr),
          seq($.IF, $.ntlexpr, $.RETURN, $.expr),
        ),
      ),
    elifs: ($) =>
      seq(
        repeat(seq($.ELIF, $.ntlexpr, $.THEN, $.blockexpr)),
        $.ELSE,
        $.blockexpr,
      ),
    valexpr: ($) => seq($.VAL, $.apattern, "=", $.blockexpr, $.IN, $.expr),
    opexpr: ($) =>
      prec.left(seq($.prefixexpr, repeat(seq($.qoperator, $.prefixexpr)))),
    prefixexpr: ($) => seq(repeat(choice("!", "~")), $.appexpr),
    appexpr: ($) =>
      prec.left(
        seq(
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
            seq("(", optional($.arguments), ")"),
            seq("[", optional($.arguments), "]"),
            seq(".", $.name),
            seq(".", "(", optional($.arguments), ")"),
          ),
        ),
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
    literal: ($) => choice($.INT, $.FLOAT, $.CHAR, $.STRING),
    mask: ($) => seq($.MASK, optional($.behind), "<", $.tbasic, ">"),
    behind: ($) => $.ID_BEHIND,
    ctxexpr: ($) => seq($.CTX, $.atom),
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
        seq(optional($.borrow), $.pattern),
        seq(optional($.borrow), $.pattern, ":", $.type),
        seq(optional($.borrow), $.pattern, ":", $.type, "=", $.expr),
        seq(optional($.borrow), $.pattern, "=", $.expr),
        seq(optional($.borrow), $.qimplicit),
        seq(optional($.borrow), $.qimplicit, ":", $.type),
      ),
    aexprs: ($) => $.aexprs1,
    aexprs1: ($) => seq($.aexpr, repeat(seq(",", $.aexpr))),
    cexprs: ($) => choice($.cexprs0, seq(optional($.cexprs0), $.aexpr)),
    cexprs0: ($) => prec.left(repeat1(seq($.aexpr, ","))),
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
    matchrules: ($) => seq($.matchrules1, $.semis1),
    matchrules1: ($) =>
      prec.left(seq($.matchrule, repeat(seq($.semis1, $.matchrule)))),
    matchrule: ($) =>
      choice(
        seq($.patterns1, "|", $.expr, $.RARROW, $.blockexpr),
        seq($.patterns1, $.RARROW, $.blockexpr),
      ),
    patterns1: ($) => seq($.pattern, repeat(seq(",", $.pattern))),
    apatterns: ($) => $.apatterns1,
    apatterns1: ($) => seq($.apattern, repeat(seq(",", $.apattern))),
    apattern: ($) => seq($.pattern, optional($.annot)),
    pattern: ($) =>
      seq(
        repeat(seq($.identifier, $.AS)),
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
        seq(optional($.override), $.HANDLER, optional($.witheff), $.opclauses),
        seq(
          optional($.override),
          $.HANDLE,
          optional($.witheff),
          $.ntlexpr,
          $.opclauses,
        ),
        seq($.NAMED, $.HANDLER, optional($.witheff), $.opclauses),
        seq($.NAMED, $.HANDLE, optional($.witheff), $.ntlexpr, $.opclauses),
      ),
    override: ($) => $.OVERRIDE,
    witheff: ($) => seq("<", $.anntype, ">"),
    withstat: ($) =>
      choice(
        seq($.WITH, $.basicexpr),
        seq($.WITH, $.binder, $.LARROW, $.basicexpr),
        seq($.WITH, optional($.override), optional($.witheff), $.opclause),
        seq($.WITH, $.binder, $.LARROW, optional($.witheff), $.opclause),
      ),
    withexpr: ($) => seq($.withstat, $.IN, $.blockexpr),
    opclauses: ($) =>
      choice(
        seq("{", optional($.semis), $.opclauses1, $.semis1, "}"),
        seq("{", optional($.semis), "}"),
      ),
    opclauses1: ($) =>
      prec.left(seq($.opclausex, repeat(seq($.semis1, $.opclausex)))),
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
        seq(
          optional($.controlmod),
          $.CTL,
          $.qidentifier,
          $.opparams,
          $.bodyexpr,
        ),
        seq($.RETURN, "(", $.opparam, ")", $.bodyexpr),
      ),
    controlmod: ($) => choice($.FINAL, $.RAW),
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
        seq($.FORALL, $.typeparams1, $.tarrow, optional($.qualifier)),
        seq($.tarrow, optional($.qualifier)),
      ),
    someforalls: ($) =>
      choice(
        seq($.SOME, $.typeparams1, $.FORALL, $.typeparams1),
        seq($.SOME, $.typeparams1),
        seq($.FORALL, $.typeparams1),
      ),
    typeparams: ($) => $.typeparams1,
    typeparams1: ($) => seq("<", optional($.tbinders), ">"),
    qualifier: ($) => seq($.WITH, "(", $.predicates1, ")"),
    predicates1: ($) => seq($.predicate, repeat(seq(",", $.predicate))),
    predicate: ($) => $.typeapp,
    tarrow: ($) => choice(seq($.tatomic, $.RARROW, $.tresult), $.tatomic),
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
        seq("(", $.RARROW, ")"),
        $.CTX,
      ),
    tparams: ($) => $.tparams1,
    tparams1: ($) => seq($.tparam, repeat(seq(",", $.tparam))),
    tparam: ($) => choice(seq($.identifier, ":", $.anntype), $.anntype),
    targuments: ($) => $.targuments1,
    targuments1: ($) => seq($.anntype, repeat(seq(",", $.anntype))),
    anntype: ($) => seq($.type, optional($.kannot)),
    kannot: ($) => seq($.DCOLON, $.kind),
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
    error: ($) => "ERROR",
  },
});
