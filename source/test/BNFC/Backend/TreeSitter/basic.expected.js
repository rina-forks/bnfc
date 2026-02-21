
module.exports = grammar({
  name: 'basic',
  rules: {
    BNFCStart: $ =>
      optional($.Start),
    Start: $ =>
      choice(
        seq("a",optional($.Start)),
        choice()
      ),
  },
});
