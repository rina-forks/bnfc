
module.exports = grammar({
  name: 'basic',
  extras: $ =>[
    /\s/,
    /\/\/.*\n/,
    /\/\*[^*]*\*([^\*\/][^*]*\*|\*)*\//,
  ],
  word: $ => $.token_Ident,
  rules: {
    BNFCStart: $ =>
      optional(
        // BNFCStart_Start. BNFCStart ::= Start
        $.Start
      ),
    Start: $ =>
      choice(
        // Start1. Start ::= Ident Start
        seq($.token_Ident, optional($.Start)),
        // Start2. Start ::=
        choice()
      ),
    token_Ident: $ =>
      /[a-zA-Z]([a-zA-Z]|\d|_|')*/,
  },
});
