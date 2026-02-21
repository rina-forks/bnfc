
module.exports = grammar({
  name: 'list',
  rules: {
    BNFCStart: $ =>
      $.Start,
    Start: $ =>
      choice(
        seq("1", $.list_NonEmptySeparator),
        seq("2", optional($.list_MaybeEmptySeparator)),
        seq("3", $.list_NonEmptyTerminator),
        seq("4", optional($.list_MaybeEmptyTerminator)),
        seq("5", optional($.list_MaybeEmptySeparatorEmptySep)),
        seq("6", optional($.list_MaybeEmptyTerminatorEmptySep)),
        seq("7", $.list_NonEmptySeparatorEmptySep),
        seq("8", $.list_NonEmptyTerminatorEmptySep)
      ),
    NonEmptySeparator: $ =>
      "a",
    list_NonEmptySeparator: $ =>
      choice(
        $.NonEmptySeparator,
        seq($.NonEmptySeparator, ",", $.list_NonEmptySeparator)
      ),
    MaybeEmptySeparator: $ =>
      "a",
    list_MaybeEmptySeparator: $ =>
      choice(
        choice(),
        $.MaybeEmptySeparator,
        seq($.MaybeEmptySeparator, ",", optional($.list_MaybeEmptySeparator))
      ),
    NonEmptyTerminator: $ =>
      "a",
    list_NonEmptyTerminator: $ =>
      choice(
        seq($.NonEmptyTerminator, ","),
        seq($.NonEmptyTerminator, ",", $.list_NonEmptyTerminator)
      ),
    MaybeEmptyTerminator: $ =>
      "a",
    list_MaybeEmptyTerminator: $ =>
      choice(
        choice(),
        seq($.MaybeEmptyTerminator, ",", optional($.list_MaybeEmptyTerminator))
      ),
    MaybeEmptySeparatorEmptySep: $ =>
      "a",
    list_MaybeEmptySeparatorEmptySep: $ =>
      choice(
        choice(),
        seq($.MaybeEmptySeparatorEmptySep, optional($.list_MaybeEmptySeparatorEmptySep))
      ),
    MaybeEmptyTerminatorEmptySep: $ =>
      "a",
    list_MaybeEmptyTerminatorEmptySep: $ =>
      choice(
        choice(),
        seq($.MaybeEmptyTerminatorEmptySep, optional($.list_MaybeEmptyTerminatorEmptySep))
      ),
    NonEmptySeparatorEmptySep: $ =>
      "a",
    list_NonEmptySeparatorEmptySep: $ =>
      choice(
        $.NonEmptySeparatorEmptySep,
        seq($.NonEmptySeparatorEmptySep, $.list_NonEmptySeparatorEmptySep)
      ),
    NonEmptyTerminatorEmptySep: $ =>
      "a",
    list_NonEmptyTerminatorEmptySep: $ =>
      choice(
        $.NonEmptyTerminatorEmptySep,
        seq($.NonEmptyTerminatorEmptySep, $.list_NonEmptyTerminatorEmptySep)
      ),
  },
});
