
module.exports = grammar({
  name: 'basic',
  rules: {
    Start: $ =>
      choice(
        "a",
        choice()
      ),
  },
});
