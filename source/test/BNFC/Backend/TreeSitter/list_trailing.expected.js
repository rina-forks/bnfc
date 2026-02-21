
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
      choice(
        choice(),
        $.A,
        seq(optional($.A), ",", optional($.list_A))
      ),
  },
});
