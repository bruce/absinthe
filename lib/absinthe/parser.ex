defmodule Absinthe.Parser do
  import NimbleParsec

  # Codepoints
  @horizontal_tab 0x0009
  @newline 0x000A
  @carriage_return 0x000D
  @space 0x0020
  @unicode_final 0xFFFF
  @unicode_bom 0xFEFF

  # SourceCharacter ::
  #   /[\u0009\u000A\u000D\u0020-\uFFFF]/
  source_character =
    utf8_char([
      @horizontal_tab,
      @newline,
      @carriage_return,
      @space..@unicode_final
    ])

  #
  # Ignored Tokens
  # Reference: http://facebook.github.io/graphql/October2016/#sec-Appendix-Grammar-Summary.Ignored-Tokens
  #

  # UnicodeBOM ::
  #   Byte Order Mark (U+FEFF)
  unicode_bom =
    utf8_char([@unicode_bom])

  # WhiteSpace ::
  #   Horizontal Tab (U+0009)
  #   Space (U+0020)
  whitespace =
    utf8_char([
      @horizontal_tab,
      @space,
    ])

  # LineTerminator ::
  #   New Line (U+000A)
  #   Carriage Return (U+000D) [lookahead != New Line (U+000A)]
  #   Carriage Return (U+000D) New Line (U+000A)
  line_terminator =
    choice([
      utf8_char([@newline]),
      utf8_char([@carriage_return])
      |> optional(utf8_char([@newline]))
    ])

  # Comment ::
  #   # CommentChar (list, opt)
  # CommentChar ::
  #   SourceCharacter but not LineTerminator
  comment =
    string("#")
    |> repeat_while(source_character, {:not_line_terminator, []})

  # Comma ::
  #   ,
  comma =
    string(",")

  # Ignored ::
  #   UnicodeBOM
  #   WhiteSpace
  #   LineTerminator
  #   Comment
  #   Comma
  ignored =
    choice([
      utf8_char([@unicode_bom]),
      whitespace,
      line_terminator,
      comment,
      comma
    ])

  #
  # Lexical Tokens
  # Reference: http://facebook.github.io/graphql/October2016/#sec-Appendix-Grammar-Summary.Lexical-Tokens
  #

  # Punctuator :: one of
  # ! $ ( ) ... : = @ [ ] { | }
  punctuator =
    choice([
      ascii_char([?!, ?$, ?(, ?), ?:, ?=, ?@, ?[, ?], ?{, ?|, ?}]),
      string("...")
    ])

  # Name ::
  #   /[_A-Za-z][_0-9A-Za-z]*/
  name =
    ascii_char([?_..?_, ?A..?Z, ?a..?z])
    |> times(ascii_char([?0..?9, ?_..?_, ?A..?Z, ?a..?z]), min: 1)

  # NegativeSign ::
  #   -
  negative_sign =
    ascii_char([?-])

  # Digit :: one of
  #   0 1 2 3 4 5 6 7 8 9
  digit =
    ascii_char([?0..?9])

  # NonZeroDigit ::
  #   Digit but not 0
  non_zero_digit =
    ascii_char([?1..?9])

  # FractionalPart ::
  #   . Digit (list)
  fractional_part =
    ascii_char([?.])
    |> times(digit, min: 1)

  # ExponentIndicator :: one of
  #   e E
  exponent_indicator =
    ascii_char([?e, ?E])

  # Sign :: one of
  #   + -
  sign =
    ascii_char([?+, ?-])

  # ExponentPart ::
  #   ExponentIndicator Sign (opt) Digit (list)
  exponent_part =
    exponent_indicator
    |> optional(sign)
    |> times(digit, min: 1)

  # IntValue ::
  #   IntegerPart
  # IntegerPart ::
  #   NegativeSign (opt) 0
  #   NegativeSign (opt) NonZeroDigit Digit (list, opt)
  integer_part =
    choice([
      optional(negative_sign) |> ascii_char([?0]),
      optional(negative_sign) |> concat(non_zero_digit) |> repeat(digit)
    ])
  int_value =
    integer_part

  # FloatValue ::
  #   IntegerPart FractionalPart
  #   IntegerPart ExponentPart
  #   IntegerPart FractionalPart ExponentPart
  float_value =
    choice([
      integer_part |> concat(fractional_part),
      integer_part |> optional(fractional_part) |> concat(exponent_part)
    ])

  defp not_line_terminator(<<?\n, _::binary>>, context, _, _), do: {:halt, context}
  defp not_line_terminator(<<?\r, _::binary>>, context, _, _), do: {:halt, context}
  defp not_line_terminator(_, context, _, _), do: {:cont, context}

  def parse(input, opts \\ []) do
    __entry__(input, opts)
  end

  defparsec :__entry__, float_value

end
