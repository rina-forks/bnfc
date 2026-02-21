
module.exports = grammar({
  name: 'list_trailing',
  rules: {
    BNFCStart: $ =>
      $.Start,
    Start: $ =>
      seq("1", optional($.list_A)),
    Opt: $ =>
      choice(
        "X",
        choice()
      ),
    A: $ =>
      $.Opt,
    list_A: $ =>
      seq($.A, repeat(seq(",", $.A))),
  },
});
