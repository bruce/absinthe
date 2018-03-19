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
  unicode_bom = utf8_char([@unicode_bom])

  # WhiteSpace ::
  #   Horizontal Tab (U+0009)
  #   Space (U+0020)
  whitespace =
    utf8_char([
      @horizontal_tab,
      @space
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

  defparsec(:__comment__, comment)

  defp not_line_terminator(<<?\n, _::binary>>, context, _, _), do: {:halt, context}
  defp not_line_terminator(<<?\r, _::binary>>, context, _, _), do: {:halt, context}
  defp not_line_terminator(_, context, _, _), do: {:cont, context}

  defp build_string(_rest, chars, context, _, _) do
    string = chars |> Enum.reverse() |> List.to_string()
    {[string], context}
  end

  # Comma ::
  #   ,
  comma = string(",")

  # Ignored ::
  #   UnicodeBOM
  #   WhiteSpace
  #   LineTerminator
  #   Comment
  #   Comma
  ignored =
    choice([
      unicode_bom,
      whitespace,
      line_terminator,
      comment,
      comma
    ])

  skip_ignored =
    empty()
    |> repeat(ignore(ignored))

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
    "..." => :ellipsis
  }

  defp build_punctuator(_rest, [punct], context, _, _) do
    {[Map.fetch!(@punctuators, punct)], context}
  end

  defparsec(:__punctuator__, punctuator)

  # Name ::
  #   /[_A-Za-z][_0-9A-Za-z]*/
  name =
    empty()
    |> concat(skip_ignored)
    |> ascii_char([?_..?_, ?A..?Z, ?a..?z])
    |> times(ascii_char([?0..?9, ?_..?_, ?A..?Z, ?a..?z]), min: 1)
    |> concat(skip_ignored)
    |> traverse({:build_string, []})

  defparsec(:__name__, name)

  # NegativeSign ::
  #   -
  negative_sign = ascii_char([?-])

  # Digit :: one of
  #   0 1 2 3 4 5 6 7 8 9
  digit = ascii_char([?0..?9])

  # NonZeroDigit ::
  #   Digit but not 0
  non_zero_digit = ascii_char([?1..?9])

  # FractionalPart ::
  #   . Digit (list)
  fractional_part =
    ascii_char([?.])
    |> times(digit, min: 1)

  # ExponentIndicator :: one of
  #   e E
  exponent_indicator = ascii_char([?e, ?E])

  # Sign :: one of
  #   + -
  sign = ascii_char([?+, ?-])

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
    optional(negative_sign)
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

  defparsec(:__int_value__, int_value)

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
      |> Enum.reverse()
      |> List.to_float()

    {[value], context}
  end

  defparsec(:__float_value__, float_value)

  @escape ?\\
  # ?"
  @quote 34

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
      ascii_char([?t]) |> replace(?\t)
    ])

  # EscapedUnicode ::
  #   /[0-9A-Fa-f]{4}
  escaped_unicode =
    times(ascii_char([?0..?9, ?A..?F, ?a..?f]), 4)
    |> traverse({:unescape_unicode, []})

  defp unescape_unicode(_rest, content, context, _line, _offset) do
    code = content |> Enum.reverse()
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

  defparsec(:__string_value__, string_value)

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

  defparsec(:__token__, token)

  #
  # Query Document
  # Reference: http://facebook.github.io/graphql/October2016/#sec-Appendix-Grammar-Summary.Query-Document
  #

  # OperationType :: one of
  #   query mutation
  #
  # Note: We also support 'subscription'
  operation_type =
    skip_ignored
    |> choice([
      string("query"),
      string("mutation"),
      string("subscription")
    ])
    |> concat(skip_ignored)
    |> map({String, :to_atom, []})

  # Alias ::
  #   Name
  alias =
    skip_ignored
    |> concat(name)
    |> concat(skip_ignored)
    |> ignore(ascii_char([?:]))
    |> concat(skip_ignored)

  # FragmentSpread ::
  #   ... FragmentName Directives (opt)

  # Argument ::
  #   Name : Value
  # argument =
  #   skip_ignored
  #   |> concat(name)
  #   |> concat(skip_ignored)
  #   |> ascii_char([?:])
  #   |> concat(skip_ignored)
  #   |> concat(value)
  #   |> concat(skip_ignored),

  # Arguments ::
  #   ( Argument (list) )
  # arguments =
  #   |> concat(skip_ignored)
  #   |> ignore(ascii_char([?(]))
  #   |> concat(skip_ignored)
  #   |> repeat(argument)
  #   |> concat(skip_ignored)
  #   |> ignore(ascii_char([?)])
  #   |> concat(skip_ignored)

  # Field ::
  #   Alias (opt) Name Arguments (opt) Directives (opt) SelectionSet (opt)
  field =
    empty()
    |> concat(skip_ignored)
    |> optional(alias |> tag(:alias))
    |> concat(skip_ignored)
    |> concat(name |> tag(:name))
    |> concat(skip_ignored)
    |> traverse({:build_field, []})

  defp build_field(_rest, values, context, _, _) do
    {
      [
        %Absinthe.Blueprint.Document.Field{
          name: tag_value(values, :name),
          alias: tag_value(values, :alias)
        }
      ],
      context
    }
  end

  # Selection ::
  #   Field
  #   FragmentSpread
  #   InlineFragment
  # TODO: choice
  selection =
    empty()
    |> times(field, min: 1)
    |> concat(skip_ignored)

  # SelectionSet ::
  #   { Selection (list) }
  selection_set =
    ignore(ascii_char([?{]))
    |> concat(skip_ignored)
    |> times(selection, min: 1)
    |> concat(skip_ignored)
    |> ignore(ascii_char([?}]))
    |> concat(skip_ignored)

  # OperationDefinition ::
  #   SelectionSet
  #   OperationType Name (opt) VariableDefinitions (opt) Directives (opt) SelectionSet
  operation_definition =
    skip_ignored
    |> choice([
      # Bare
      selection_set |> tag(:selections),
      # Normal
      operation_type
      |> tag(:operation_type)
      |> optional(name |> tag(:name))
      |> concat(selection_set |> tag(:selections))
    ])
    |> concat(skip_ignored)
    |> traverse({:build_operation, []})

  defp build_operation(_rest, values, context, _, _) do
    {
      [
        %Absinthe.Blueprint.Document.Operation{
          name: tag_value(values, :name),
          type: tag_value(values, :operation_type, :query),
          selections: tag_value_list(values, :selections)
        }
      ],
      context
    }
  end

  # ExecutableDefinition ::
  #  OperationDefinition
  #  FragmentDefinition
  executable_definition =
    empty()
    |> concat(operation_definition)

  # Definition ::
  #   ExecutableDefinition
  #   TypeSystemDefinition
  # TODO: Expand
  definition =
    empty()
    |> concat(executable_definition)

  # Document ::
  #   Definition (list)
  document =
    empty()
    |> concat(skip_ignored)
    |> repeat(definition)
    |> concat(skip_ignored)
    |> traverse({:build_blueprint, []})

  defp build_blueprint("", definitions, context, _, _) do
    {[do_build_blueprint(definitions)], context}
  end

  defp build_blueprint(_rest, _value, _context, _, _) do
    {:error, "Parse error"}
  end

  defp do_build_blueprint(definitions) do
    Enum.reduce(definitions, %Absinthe.Blueprint{}, fn
      %Absinthe.Blueprint.Document.Operation{} = defn, acc ->
        %{acc | operations: [defn | acc.operations]}

      %Absinthe.Blueprint.Document.Fragment.Named{} = defn, acc ->
        %{acc | fragments: [defn | acc.fragments]}
    end)
  end

  defparsec(:__document__, document)

  def parse(input) do
    case __document__(input) do
      {:ok, [doc], "", _, _, _} ->
        {:ok, doc}
    end
  end

  defp tag_value(values, key, default \\ nil) do
    case values[key] do
      nil ->
        default

      [value] ->
        value
    end
  end

  defp tag_value_list(values, key) do
    case values[key] do
      nil ->
        []

      list ->
        list
    end
  end
end
