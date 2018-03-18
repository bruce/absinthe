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
    ignore(string("#"))
    |> repeat_while(source_character, {:not_line_terminator, []})
    |> traverse({:build_string, []})

  defparsec :__comment__, comment

  defp not_line_terminator(<<?\n, _::binary>>, context, _, _), do: {:halt, context}
  defp not_line_terminator(<<?\r, _::binary>>, context, _, _), do: {:halt, context}
  defp not_line_terminator(_, context, _, _), do: {:cont, context}

  defp build_string(_rest, chars, context, _, _) do
    string = chars |> Enum.reverse |> List.to_string
    {[string], context}
  end

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
    |> traverse({:build_punctuator, []})

  @punctuators %{
    ?! => :exclamation,
    ?$ => :dollar,
    ?( => :left_paren,
    ?) => :right_paren,
    ?: => :colon,
    ?= => :equal,
    ?@ => :at,
    ?[ => :left_bracket,
    ?] => :right_bracket,
    ?{ => :left_brace,
    ?} => :right_brace,
    ?| => :pipe,
    "..." => :ellipsis,
  }

  defp build_punctuator(_rest, [punct], context, _, _) do
    {[Map.fetch!(@punctuators, punct)], context}
  end

  defparsec :__punctuator__, punctuator

  # Name ::
  #   /[_A-Za-z][_0-9A-Za-z]*/
  name =
    ascii_char([?_..?_, ?A..?Z, ?a..?z])
    |> times(ascii_char([?0..?9, ?_..?_, ?A..?Z, ?a..?z]), min: 1)
    |> traverse({:build_string, []})

  defparsec :__name__, name

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

  # IntegerPart ::
  #   NegativeSign (opt) 0
  #   NegativeSign (opt) NonZeroDigit Digit (list, opt)
  integer_part =
    optional(ascii_char([?-]))
    |> choice([
      ascii_char([?0]),
      non_zero_digit |> repeat(digit)
    ])

  int_value =
    empty()
    |> concat(integer_part)
    |> traverse({:build_int_value, []})

  defp build_int_value(rest, value, context, line, offset) do
    do_build_int_value(rest, Enum.reverse(value), context, line, offset)
  end

  defp do_build_int_value(_rest, [?- | digits], context, _, _) do
    {[List.to_integer(digits) * -1], context}
  end
  defp do_build_int_value(_rest, digits, context, _, _) do
    {[List.to_integer(digits)], context}
  end

  defparsec :__int_value__, int_value

  # FloatValue ::
  #   IntegerPart FractionalPart
  #   IntegerPart ExponentPart
  #   IntegerPart FractionalPart ExponentPart
  float_value =
    choice([
      integer_part |> concat(fractional_part) |> concat(exponent_part),
      integer_part |> traverse({:fill_mantissa, []}) |> concat(exponent_part),
      integer_part |> concat(fractional_part)
    ])
    |> traverse({:build_float_value, []})

  # GraphQL allows notation like `1e3` as shorthand for `1.0e3`.
  # This normalizes by adding in the `.0`.
  defp fill_mantissa(_rest, raw, context, _, _), do: {'0.' ++ raw, context}

  defp build_float_value(_rest, value, context, _line, _offset) do
    value =
      value
      |> Enum.reverse
      |> List.to_float
    {[value], context}
  end

  defparsec :__float_value__, float_value

  @escape ?\\
  @quote 34 # ?"

  # EscapedCharacter :: one of
  #   " \ / b f n r t
  escaped_character =
    choice([
      ascii_char([@quote]),
      ascii_char([?\\]),
      ascii_char([?/]),
      ascii_char([?b]) |> replace(?\b),
      ascii_char([?f]) |> replace(?\f),
      ascii_char([?n]) |> replace(?\n),
      ascii_char([?r]) |> replace(?\r),
      ascii_char([?t]) |> replace(?\t),
    ])

  # EscapedUnicode ::
  #   /[0-9A-Fa-f]{4}
  escaped_unicode =
    times(ascii_char([?0..?9, ?A..?F, ?a..?f]), 4)
    |> traverse({:unescape_unicode, []})

  defp unescape_unicode(_rest, content, context, _line, _offset) do
    code = content |> Enum.reverse
    value = :httpd_util.hexlist_to_integer(code)
    binary = :unicode.characters_to_binary([value])
    {[binary], context}
  end

  # StringCharacter ::
  #   SourceCharacter but not " or \ or LineTerminator
  #   \u EscapedUnicode
  #   \ EscapedCharacter
  string_character =
    choice([
      ignore(string(~S(\u))) |> concat(escaped_unicode),
      ignore(ascii_char([@escape])) |> concat(escaped_character),
      source_character
    ])

  # StringValue ::
  #   ""
  #   " StringCharacter (list) "
  string_value =
    ignore(ascii_char([@quote]))
    |> repeat_while(string_character, {:not_end_of_quote, []})
    |> ignore(ascii_char([@quote]))
    |> traverse({:build_string, []})

  defparsec :__string_value__, string_value

  defp not_end_of_quote(<<@quote, _::binary>>, context, _, _) do
    {:halt, context}
  end
  defp not_end_of_quote(rest, context, current_line, current_offset) do
    not_line_terminator(rest, context, current_line, current_offset)
  end

  # Token ::
  #   Punctuator
  #   Name
  #   IntValue
  #   FloatValue
  #   StringValue
  token =
    choice([
      punctuator,
      name,
      float_value,
      int_value,
      string_value
    ])

  defparsec :__token__, token

  def parse(input, opts \\ []) do
    __entry__(input, opts)
  end

  defparsec :__entry__, token

end
