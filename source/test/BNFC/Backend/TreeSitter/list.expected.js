
module.exports = grammar({
  name: 'list',
  rules: {
    Start: $ =>
      choice(
        seq("1",$.list_NonEmptySeparator),
        seq("2",optional($.list_MaybeEmptySeparator)),
        seq("3",$.list_NonEmptyTerminator),
        seq("4",optional($.list_MaybeEmptyTerminator)),
        seq("5",optional($.list_MaybeEmptySeparatorEmptySep)),
        seq("6",optional($.list_MaybeEmptyTerminatorEmptySep)),
        seq("7",$.list_NonEmptySeparatorEmptySep),
        seq("8",$.list_NonEmptyTerminatorEmptySep)
      ),
    NonEmptySeparator: $ =>
      "a",
    list_NonEmptySeparator: $ =>
      seq($.NonEmptySeparator,repeat(seq(",",$.NonEmptySeparator))),
    MaybeEmptySeparator: $ =>
      "a",
    list_MaybeEmptySeparator: $ =>
      seq($.MaybeEmptySeparator,repeat(seq(",",$.MaybeEmptySeparator))),
    NonEmptyTerminator: $ =>
      "a",
    list_NonEmptyTerminator: $ =>
      repeat1(seq($.NonEmptyTerminator,",")),
    MaybeEmptyTerminator: $ =>
      "a",
    list_MaybeEmptyTerminator: $ =>
      repeat1(seq($.MaybeEmptyTerminator,",")),
    MaybeEmptySeparatorEmptySep: $ =>
      "a",
    list_MaybeEmptySeparatorEmptySep: $ =>
      repeat1($.MaybeEmptySeparatorEmptySep),
    MaybeEmptyTerminatorEmptySep: $ =>
      "a",
    list_MaybeEmptyTerminatorEmptySep: $ =>
      repeat1($.MaybeEmptyTerminatorEmptySep),
    NonEmptySeparatorEmptySep: $ =>
      "a",
    list_NonEmptySeparatorEmptySep: $ =>
      repeat1($.NonEmptySeparatorEmptySep),
    NonEmptyTerminatorEmptySep: $ =>
      "a",
    list_NonEmptyTerminatorEmptySep: $ =>
      repeat1($.NonEmptyTerminatorEmptySep),
  },
});
