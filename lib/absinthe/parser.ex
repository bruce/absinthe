defmodule Absinthe.Parser do
  import NimbleParsec

  # Codepoints
  @horizontal_tab 0x0009
  @newline 0x000A
  @carriage_return 0x000D
  @space 0x0020
  @unicode_final 0xFFFF
  @unicode_bom 0xFEFF

  # SourceCharacter :: /[\u0009\u000A\u000D\u0020-\uFFFF]/
  source_character =
    utf8_char([
      @horizontal_tab,
      @newline,
      @carriage_return,
      @space..@unicode_final
    ])

  # ## Ignored Tokens

  # UnicodeBOM :: "Byte Order Mark (U+FEFF)"
  unicode_bom = utf8_char([@unicode_bom])

  # WhiteSpace ::
  #   - "Horizontal Tab (U+0009)"
  #   - "Space (U+0020)"
  whitespace =
    utf8_char([
      @horizontal_tab,
      @space
    ])

  # LineTerminator ::
  #   - "New Line (U+000A)"
  #   - "Carriage Return (U+000D)" [ lookahead ! "New Line (U+000A)" ]
  #   - "Carriage Return (U+000D)" "New Line (U+000A)"
  line_terminator =
    choice([
      utf8_char([@newline]),
      utf8_char([@carriage_return])
      |> optional(utf8_char([@newline]))
    ])

  # Comment :: `#` CommentChar*
  # CommentChar :: SourceCharacter but not LineTerminator
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

  # Comma :: ,
  comma = string(",")

  # Ignored ::
  #   - UnicodeBOM
  #   - WhiteSpace
  #   - LineTerminator
  #   - Comment
  #   - Comma
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

  # ## Lexical Tokens

  # Token ::
  #   - Punctuator
  #   - Name
  #   - IntValue
  #   - FloatValue
  #   - StringValue

  # Punctuator :: one of ! $ ( ) ... : = @ [ ] { | }

  # Name :: /[_A-Za-z][_0-9A-Za-z]*/
  name =
    empty()
    |> concat(skip_ignored)
    |> ascii_char([?_..?_, ?A..?Z, ?a..?z])
    |> times(ascii_char([?0..?9, ?_..?_, ?A..?Z, ?a..?z]), min: 1)
    |> concat(skip_ignored)
    |> traverse({:build_string, []})

  defparsec(:__name__, name)

  # NegativeSign :: -
  negative_sign = ascii_char([?-])

  # Digit :: one of 0 1 2 3 4 5 6 7 8 9
  digit = ascii_char([?0..?9])

  # NonZeroDigit :: Digit but not `0`
  non_zero_digit = ascii_char([?1..?9])  

  # IntegerPart ::
  #   - NegativeSign? 0
  #   - NegativeSign? NonZeroDigit Digit*
  integer_part =
    optional(negative_sign)
    |> choice([
      ascii_char([?0]),
      non_zero_digit |> repeat(digit)
    ])

  # IntValue :: IntegerPart
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

  # FractionalPart :: . Digit+
  fractional_part =
    ascii_char([?.])
    |> times(digit, min: 1)  

  # ExponentIndicator :: one of `e` `E`
  exponent_indicator = ascii_char([?e, ?E])

  # Sign :: one of + -
  sign = ascii_char([?+, ?-])  

  # ExponentPart :: ExponentIndicator Sign? Digit+
  exponent_part =
    exponent_indicator
    |> optional(sign)
    |> times(digit, min: 1)

  # FloatValue ::
  #   - IntegerPart FractionalPart
  #   - IntegerPart ExponentPart
  #   - IntegerPart FractionalPart ExponentPart
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

  # EscapedUnicode :: /[0-9A-Fa-f]{4}/
  escaped_unicode =
    times(ascii_char([?0..?9, ?A..?F, ?a..?f]), 4)
    |> traverse({:unescape_unicode, []})

  defp unescape_unicode(_rest, content, context, _line, _offset) do
    code = content |> Enum.reverse()
    value = :httpd_util.hexlist_to_integer(code)
    binary = :unicode.characters_to_binary([value])
    {[binary], context}
  end  

  # EscapedCharacter :: one of `"` \ `/` b f n r t
  escaped_character =
    choice([
      ascii_char([?"]),
      ascii_char([?\\]),
      ascii_char([?/]),
      ascii_char([?b]) |> replace(?\b),
      ascii_char([?f]) |> replace(?\f),
      ascii_char([?n]) |> replace(?\n),
      ascii_char([?r]) |> replace(?\r),
      ascii_char([?t]) |> replace(?\t)
    ]) 

  # StringCharacter ::
  #   - SourceCharacter but not `"` or \ or LineTerminator
  #   - \u EscapedUnicode
  #   - \ EscapedCharacter
  string_character =
    choice([
      ignore(string(~S(\u))) |> concat(escaped_unicode),
      ignore(ascii_char([?\\])) |> concat(escaped_character),
      source_character
    ])

  # BlockStringCharacter ::
  #   - SourceCharacter but not `"""` or `\"""`
  #   - `\"""`

  # Note: Block string values are interpreted to exclude blank initial and trailing
  # lines and uniform indentation with {BlockStringValue()}.
  block_string_character =
    string("BLOCK_STRING_STUB")

  # StringValue ::
  #   - `"` StringCharacter* `"`
  #   - `"""` BlockStringCharacter* `"""`
  string_value =
    ignore(ascii_char([?"]))
    |> repeat_while(string_character, {:not_end_of_quote, []})
    |> ignore(ascii_char([?"]))
    |> traverse({:build_string, []})
    # TODO: Block quote

  defparsec(:__string_value__, string_value)

  defp not_end_of_quote(<<?", _::binary>>, context, _, _) do
    {:halt, context}
  end

  defp not_end_of_quote(rest, context, current_line, current_offset) do
    not_line_terminator(rest, context, current_line, current_offset)
  end    

  # ## Document      

  # OperationType : one of query mutation subscription
  operation_type =
    skip_ignored
    |> choice([
      string("query"),
      string("mutation"),
      string("subscription")
    ])
    |> concat(skip_ignored)
    |> map({String, :to_atom, []})

  # Alias : Name :
  alias =
    skip_ignored
    |> concat(name)
    |> concat(skip_ignored)
    |> ignore(ascii_char([?:]))
    |> concat(skip_ignored)

  # Field : Alias? Name Arguments? Directives? SelectionSet?
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

  # FragmentName : Name but not `on`
  fragment_name =
    empty()
    |> concat(skip_ignored)
    |> concat(name)
    |> concat(skip_ignored)
    |> traverse({:check_fragment_name, []})

  defp check_fragment_name(_rest, ["on"], _context, _line, _offset) do
    {:error, ~S(Invalid fragment name 'on')}
  end

  defp check_fragment_name(_rest, values, context, _, _) do
    {values, context}
  end

  # NamedType : Name
  named_type =
    name

  # TypeCondition : on NamedType
  type_condition =
    string("on")
    |> concat(named_type)

  # BooleanValue : one of `true` `false`
  boolean_value =
    choice([
      string("true"),

      string("false")
    ])

  # NullValue : `null`
  null_value = string("null")

  # EnumValue : Name but not `true`, `false` or `null`
  enum_value =
    name
    |> lookahead({:error_when_next_is_not_enum_value, []})

  for text <- ~w(true false null) do
    defp error_when_next_is_not_enum_value(<<unquote(text), _::binary>>, _context, _line, _offset) do
      {:error, "not a valid enum value"}
    end
  end 

  # ListValue[Const] :
  #   - [ ]
  #   - [ Value[?Const]+ ]
  list_value =
    choice([
      ascii_char([?[])
      |> ascii_char([?]]),

      ascii_char([?[])
      |> times(parsec(:__value__), min: 1)
      |> ascii_char([?]])
    ])

  # ObjectField[Const] : Name : Value[?Const]
  object_field =
    name
    |> ascii_char([?:])
    |> concat(parsec(:__value__))

  # ObjectValue[Const] :
  #   - { }
  #   - { ObjectField[?Const]+ }
  object_value =
    choice([
      ascii_char([?{])
      |> ascii_char([?{]),

      ascii_char([?{])
      |> times(object_field, min: 1)
      |> ascii_char([?{])
    ])

  # Variable : $ Name
  variable =
    ascii_char([?$])
    |> concat(name)

  # Value[Const] :
  #   - [~Const] Variable
  #   - IntValue
  #   - FloatValue
  #   - StringValue
  #   - BooleanValue
  #   - NullValue
  #   - EnumValue
  #   - ListValue[?Const]
  #   - ObjectValue[?Const]
  value =
    choice([
      variable,
      int_value,
      float_value,
      string_value,
      boolean_value,
      null_value,
      enum_value,
      list_value,
      object_value
    ])
  defparsec :__value__, value

  # Argument[Const] : Name : Value[?Const]
  argument =
    name
    |> ascii_char([?:])
    |> concat(value)

  # Arguments[Const] : ( Argument[?Const]+ )
  arguments =
    ascii_char([?(])
    |> times(argument, min: 1)
    |> ascii_char([?)])

  # Directive[Const] : @ Name Arguments[?Const]?
  directive =
    ascii_char([?@])
    |> concat(name)
    |> optional(arguments)

  # Directives[Const] : Directive[?Const]+
  directives =
    times(directive, min: 1)

  # FragmentDefinition : fragment FragmentName TypeCondition Directives? SelectionSet
  fragment_definition =
    string("fragment")
    |> concat(fragment_name)
    |> concat(type_condition)
    |> optional(directives)
    |> concat(parsec(:__selection_set__))

  # InlineFragment : ... TypeCondition? Directives? SelectionSet
  inline_fragment =
    string("...")
    |> optional(type_condition)
    |> optional(directives)
    |> concat(parsec(:__selection_set__))

  # FragmentSpread : ... FragmentName Directives?
  fragment_spread =
    empty()
    |> concat(skip_ignored)
    |> string("...")
    |> concat(fragment_name |> tag(:name))
    |> concat(skip_ignored)
    |> traverse({:build_fragment_spread, []})

  defp build_fragment_spread(_rest, values, context, _, _) do
    {
      [%Absinthe.Blueprint.Document.Fragment.Spread{name: tag_value(values, :name)}],
      context
    }
  end

  # Selection :
  #   - Field
  #   - FragmentSpread
  #   - InlineFragment
  selection =
    empty()
    |> times(choice([field, fragment_spread]), min: 1)
    |> concat(skip_ignored)

  # SelectionSet : { Selection+ }
  selection_set =
    ignore(ascii_char([?{]))
    |> concat(skip_ignored)
    |> times(selection, min: 1)
    |> concat(skip_ignored)
    |> ignore(ascii_char([?}]))
    |> concat(skip_ignored)

  defparsec :__selection_set__, selection_set

  # OperationDefinition :
  #   - SelectionSet
  #   - OperationType Name? VariableDefinitions? Directives? SelectionSet
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

  # ExecutableDefinition :
  #   - OperationDefinition
  #   - FragmentDefinition
  executable_definition =
    empty()
    |> choice([
      operation_definition,
      fragment_definition
    ])

  # DefaultValue : = Value[Const]
  default_value =
    ascii_char([?=])
    |> concat(value)

  # VariableDefinition : Variable : Type DefaultValue?
  variable_definition =
    variable
    |> ascii_char([?:])
    |> concat(parsec(:__type__))
    |> optional(default_value)

  # VariableDefinitions : ( VariableDefinition+ )
  variable_definitions =
    ascii_char([?(])
    |> times(variable_definition, min: 1)
    |> ascii_char([?)])

  # ListType : [ Type ]
  list_type =
    ascii_char([?[])
    |> concat(parsec(:__type__))
    |> ascii_char([?]])

  # NonNullType :
  #   - NamedType !
  #   - ListType !
  non_null_type =
    choice([
      named_type
      |> ascii_char([?!]),

      list_type
      |> ascii_char([?!])
    ])

  # Type :
  #   - NamedType
  #   - ListType
  #   - NonNullType
  type =
    choice([
      named_type,
      list_type,
      non_null_type
    ])

  defparsec :__type__, type

  # OperationTypeDefinition : OperationType : NamedType
  operation_type_definition =
    operation_type
    |> ascii_char([?:])
    |> concat(named_type)

  # SchemaDefinition : schema Directives[Const]? { OperationTypeDefinition+ }
  schema_definition =
    string("schema")
    |> optional(directives)
    |> ascii_char([?{])
    |> times(operation_type_definition, min: 1)
    |> ascii_char([?}])

  # Description : StringValue
  description =
    string_value

  # ScalarTypeDefinition : Description? scalar Name Directives[Const]?
  scalar_type_definition =
    optional(description)
    |> string("scalar")
    |> concat(name)
    |> optional(directives)

  # ScalarTypeExtension :
  #   - extend scalar Name Directives[Const]
  scalar_type_extension =
    string("extend")
    |> string("scalar")
    |> concat(name)
    |> concat(directives)

  # ImplementsInterfaces :
  #   - implements `&`? NamedType
  #   - ImplementsInterfaces & NamedType
  implements_interfaces =
    choice([
      string("implements")
      |> optional(ascii_char([?&]))
      |> concat(named_type),

      parsec(:__implements_interfaces__)
      |> ascii_char([?&])
      |> concat(named_type)
    ])

  defparsec :__implements_interfaces__, implements_interfaces

  # InputValueDefinition : Description? Name : Type DefaultValue? Directives[Const]?
  input_value_definition =
    optional(description)
    |> concat(name)
    |> ascii_char([?:])
    |> concat(type)
    |> optional(default_value)
    |> optional(directives)

  # ArgumentsDefinition : ( InputValueDefinition+ )
  arguments_definition =
    ascii_char([?(])
    |> times(input_value_definition, min: 1)
    |> ascii_char([?)])

  # FieldDefinition : Description? Name ArgumentsDefinition? : Type Directives[Const]?
  field_definition =
    optional(description)
    |> concat(name)
    |> optional(arguments_definition)
    |> ascii_char([?:])
    |> optional(directives)

  # FieldsDefinition : { FieldDefinition+ }
  fields_definition =
    ascii_char([?{])
    |> times(field_definition, min: 1)
    |> ascii_char([?}])

  # ObjectTypeExtension :
  #   - extend type Name ImplementsInterfaces? Directives[Const]? FieldsDefinition
  #   - extend type Name ImplementsInterfaces? Directives[Const]
  #   - extend type Name ImplementsInterfaces
  object_type_extension =
    choice([
      string("extend")
      |> string("type")
      |> concat(name)
      |> optional(implements_interfaces)
      |> optional(directives)
      |> concat(fields_definition),

      string("extend")
      |> string("type")
      |> concat(name)
      |> optional(implements_interfaces)
      |> concat(directives),

      string("extend")
      |> string("type")
      |> concat(name)
      |> concat(implements_interfaces)
    ])

  # ObjectTypeDefinition : Description? type Name ImplementsInterfaces? Directives[Const]? FieldsDefinition?
  object_type_definition =
    optional(description)
    |> string("type")
    |> concat(name)
    |> optional(implements_interfaces)
    |> optional(directives)
    |> optional(fields_definition)


  # InterfaceTypeDefinition : Description? interface Name Directives[Const]? FieldsDefinition?
  interface_type_definition =
    optional(description)
    |> string("interface")
    |> concat(name)
    |> optional(directives)
    |> optional(fields_definition)

  # InterfaceTypeExtension :
  #   - extend interface Name Directives[Const]? FieldsDefinition
  #   - extend interface Name Directives[Const]
  interface_type_extension =
    choice([
      string("extend")
      |> string("interface")
      |> concat(name)
      |> optional(directives)
      |> concat(fields_definition),

      string("extend")
      |> string("interface")
      |> concat(name)
      |> concat(directives)
    ])

  # UnionMemberTypes :
  #   - = `|`? NamedType
  #   - UnionMemberTypes | NamedType
  union_member_types =
    choice([
      ascii_char([?=])
      |> optional(ascii_char([?|]))
      |> concat(named_type),

      parsec(:__union_member_types__)
      |> ascii_char([?|])
      |> concat(named_type)
    ])

  defparsec :__union_member_types__, union_member_types

  # UnionTypeDefinition : Description? union Name Directives[Const]? UnionMemberTypes?
  union_type_definition =
    optional(description)
    |> string("union")
    |> concat(name)
    |> optional(directives)
    |> optional(union_member_types)

  # UnionTypeExtension :
  #   - extend union Name Directives[Const]? UnionMemberTypes
  #   - extend union Name Directives[Const]
  union_type_extension =
    choice([
      string("extend")
      |> string("union")
      |> concat(name)
      |> optional(directives)
      |> concat(union_member_types),

      string("extend")
      |> string("union")
      |> concat(name)
      |> concat(directives)
    ])

  # EnumValueDefinition : Description? EnumValue Directives[Const]?
  enum_value_definition =
    optional(description)
    |> concat(enum_value)
    |> optional(directives)

  # EnumValuesDefinition : { EnumValueDefinition+ }
  enum_values_definition =
    ascii_char([?{])
    |> times(enum_value_definition, min: 1)
    |> ascii_char([?}])

  # EnumTypeDefinition : Description? enum Name Directives[Const]? EnumValuesDefinition?
  enum_type_definition =
    optional(description)
    |> string("enum")
    |> concat(name)
    |> optional(directives)
    |> optional(enum_values_definition)

  # EnumTypeExtension :
  #   - extend enum Name Directives[Const]? EnumValuesDefinition
  #   - extend enum Name Directives[Const]
  enum_type_extension =
    choice([
      string("extend")
      |> string("enum")
      |> concat(name)
      |> optional(directives)
      |> concat(enum_values_definition),

      string("extend")
      |> string("enum")
      |> concat(name)
      |> concat(directives)
    ])

  # InputFieldsDefinition : { InputValueDefinition+ }
  input_fields_definition =
    ascii_char([?{])
    |> times(input_value_definition, min: 1)
    |> ascii_char([?}])

  # InputObjectTypeDefinition : Description? input Name Directives[Const]? InputFieldsDefinition?
  input_object_type_definition =
    optional(description)
    |> string("input")
    |> concat(name)
    |> optional(directives)
    |> optional(input_fields_definition)

  # InputObjectTypeExtension :
  #   - extend input Name Directives[Const]? InputFieldsDefinition
  #   - extend input Name Directives[Const]
  input_object_type_extension =
    choice([
      string("extend")
      |> string("input")
      |> concat(name)
      |> optional(directives)
      |> concat(input_fields_definition),
      
      string("extend")
      |> string("input")
      |> concat(name)
      |> concat(directives)
    ])

  # ExecutableDirectiveLocation : one of
  #   `QUERY`
  #   `MUTATION`
  #   `SUBSCRIPTION`
  #   `FIELD`
  #   `FRAGMENT_DEFINITION`
  #   `FRAGMENT_SPREAD`
  #   `INLINE_FRAGMENT`
  executable_directive_location =
    choice([
      string("QUERY"),
      string("MUTATION"),
      string("SUBSCRIPTION"),
      string("FIELD"),
      string("FRAGMENT_DEFINITION"),
      string("FRAGMENT_SPREAD"),
      string("INLINE_FRAGMENT")
    ])

  # TypeSystemDirectiveLocation : one of
  #   `SCHEMA`
  #   `SCALAR`
  #   `OBJECT`
  #   `FIELD_DEFINITION`
  #   `ARGUMENT_DEFINITION`
  #   `INTERFACE`
  #   `UNION`
  #   `ENUM`
  #   `ENUM_VALUE`
  #   `INPUT_OBJECT`
  #   `INPUT_FIELD_DEFINITION`
  type_system_directive_location =
    choice([
      string("SCHEMA"),
      string("SCALAR"),
      string("OBJECT"),
      string("FIELD_DEFINITION"),
      string("ARGUMENT_DEFINITION"),
      string("INTERFACE"),
      string("UNION"),
      string("ENUM"),
      string("ENUM_VALUE"),
      string("INPUT_OBJECT"),
      string("INPUT_FIELD_DEFINITION")
    ])

  # DirectiveLocation :
  #   - ExecutableDirectiveLocation
  #   - TypeSystemDirectiveLocation
  directive_location =
    choice([
      executable_directive_location,

      type_system_directive_location
    ])

  # DirectiveLocations :
  #   - `|`? DirectiveLocation
  #   - DirectiveLocations | DirectiveLocation
  directive_locations =
    choice([
      ignore(ascii_char([?|]))
      |> concat(directive_location),

      parsec(:__directive_locations__)
      |> ignore(ascii_char([?|]))
      |> concat(directive_location)
    ])

  defparsec :__directive_locations__, directive_locations


  # DirectiveDefinition : Description? directive @ Name ArgumentsDefinition? on DirectiveLocations
  directive_definition =
    optional(description)
    |> string("directive")
    |> ascii_char([?@])
    |> concat(name)
    |> optional(arguments_definition)
    |> string("on")    
    |> concat(directive_locations)

  # TypeDefinition :
  #   - ScalarTypeDefinition
  #   - ObjectTypeDefinition
  #   - InterfaceTypeDefinition
  #   - UnionTypeDefinition
  #   - EnumTypeDefinition
  #   - InputObjectTypeDefinition
  type_definition =
    choice([
      scalar_type_definition,
      object_type_definition,
      interface_type_definition,
      union_type_definition,
      enum_type_definition,
      input_object_type_definition
    ])

  # TypeExtension :
  #   - ScalarTypeExtension
  #   - ObjectTypeExtension
  #   - InterfaceTypeExtension
  #   - UnionTypeExtension
  #   - EnumTypeExtension
  #   - InputObjectTypeExtension
  type_extension =
    choice([
      scalar_type_extension,
      object_type_extension,
      interface_type_extension,
      union_type_extension,
      enum_type_extension,
      input_object_type_extension
    ])

  # TypeSystemDefinition :
  #   - SchemaDefinition
  #   - TypeDefinition
  #   - TypeExtension
  #   - DirectiveDefinition
  type_system_definition =
    choice([
      schema_definition,
      type_definition,
      type_extension,
      directive_definition
    ])

  # Definition :
  #   - ExecutableDefinition
  #   - TypeSystemDefinition
  definition =
    choice([
      executable_definition,
      type_system_definition
    ])

  # Document : Definition+
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

      {:error, err, _, _, _, _} ->
        {:error, err}
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
