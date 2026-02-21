
module.exports = grammar({
  name: 'basic',
  rules: {
    Start: $ =>
      choice(
        seq("a",optional($.Start)),
        choice()
      ),
  },
});
