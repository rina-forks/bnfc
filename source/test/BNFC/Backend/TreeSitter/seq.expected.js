
module.exports = grammar({
  name: 'seq',
  rules: {
    BNFCStart: $ =>
      optional(
        $.Start
      ),
    Start: $ =>
      choice(
        $.TransitivelyOptional,
        $.NonOptional
      ),
    TransitivelyOptional: $ =>
      choice(
        seq($.A,optional($.B),optional($.C)),
        seq($.B,optional($.C)),
        $.C
      ),
    A: $ =>
      choice(
        "a",
        choice()
      ),
    B: $ =>
      choice(
        "b",
        choice()
      ),
    C: $ =>
      choice(
        "c",
        choice()
      ),
    NonOptional: $ =>
      seq("x",optional($.A),optional($.B),optional($.C)),
  },
});
