
module.exports = grammar({
  name: 'basic',
  word: $ => $.token_Ident,
  rules: {
    BNFCStart: $ =>
      optional(
        $.Start
      ),
    Start: $ =>
      choice(
        seq($.token_Ident, optional($.Start)),
        choice()
      ),
    token_Ident: $ => /[a-zA-Z][a-zA-Z\d_']*/,
  },
});
