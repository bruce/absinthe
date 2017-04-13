defmodule Absinthe.Parser do

  alias Absinthe.Blueprint
  import Absinthe.Parser.Notation

  @enforce_keys [:input, :rest]
  defstruct [
    :input,
    :rest,
    cursor: {1, 1},
    stack: [],
    scopes: [],
  ]

  @type t :: %__MODULE__{
    input: String.t,
    rest: String.t,
    cursor: {integer, integer},
    stack: [Blueprint.node_t],
    scopes: [atom],
  }

  defp new(input) do
    %__MODULE__{
      input: input,
      rest: input,
      stack: [%Blueprint{}],
      scopes: [:in_document]
    }
  end

  def run(text) do
    new(text)
    |> do_run
  end

  def do_run(%{scopes: [scope_to_run | _]} = state) do
    case apply(__MODULE__, scope_to_run, [state]) do
      {:next, next_scopes, state} when is_list(next_scopes) ->
        %{state | scopes: next_scopes ++ state.scopes}
        |> do_run
      {:next, next_scope, state} ->
        %{state | scopes: [next_scope | state.scopes]}
        |> do_run
      {:prev, %{scopes: [_ | previous_scopes]} = state} ->
        %{state | scopes: previous_scopes}
        |> do_run
      {:ok, _, _} = result ->
        result
      {:error, _, _} = result ->
        result
    end
  end

  @name_first '_ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'
  @name_rest '_0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'
  @digits '0123456789'

  @start_number '-0123456789'

  #
  # DOCUMENT
  #

  def in_document(%{rest: "", stack: [%Blueprint{} = blueprint]} = state) do
    {:ok, blueprint, state}
  end
  def in_document(%{rest: "{" <> rest} = state) do
    op =
      %Blueprint.Document.Operation{type: :query, name: nil}
      |> located(state)
    %{
      state |
      rest: rest,
      stack: [op | state.stack],
    }
    |> move(1)
    # Skip :in_operation
    |> next([:in_fields, :in_operation])
  end
  def in_document(%{rest: "", stack: stack} = state) when length(stack) > 1 do
    {:error, "unbalanced", state}
  end
  def in_document(%{rest: "query" <> rest} = state) do
    add_operation_in_document(state, rest, :query, 5)
  end
  def in_document(%{rest: "mutation" <> rest} = state) do
    add_operation_in_document(state, rest, :mutation, 8)
  end
  def in_document(%{rest: "subscription" <> rest} = state) do
    add_operation_in_document(state, rest, :subscription, 12)
  end
  skip_whitespace :in_document
  end_rules :in_document

  defp add_operation_in_document(state, rest, type, offset) do
    op =
      %Blueprint.Document.Operation{type: type, name: nil}
      |> located(state)
    %{
      state |
      rest: rest,
      stack: [op | state.stack],
      cursor: move(state.cursor, offset),
    }
    |> next(:in_operation)
  end

  #
  # OPERATION
  #

  def in_operation(%{rest: "}" <> rest} = state) do
    %{state | rest: rest}
    |> move(1)
    |> collapse_down(Blueprint.Document.Operation, :operations)
    |> prev
  end
  def in_operation(%{rest: "{" <> rest} = state) do
    %{state | rest: rest}
    |> move(1)
    |> next(:in_fields)
  end
  skip_whitespace :in_operation
  def in_operation(%{rest: "(" <> rest} = state) do
    %{state | rest: rest}
    |> move(1)
    |> next(:in_variable_definitions)
  end
  def in_operation(%{rest: <<letter::size(8), rest::binary>>, stack: [op | stack]} = state) when letter in @name_first do
    {state, name} =
      %{state | rest: rest}
      |> move(1)
      |> collect_rest_of_name(<<letter::size(8)>>)
    %{state | stack: [%{op | name: name} | stack]}
    |> in_operation
  end
  end_rules :in_operation

  #
  # VARIABLE DEFINITIONS
  #

  def in_variable_definitions(%{rest: "$" <> rest} = state) do
    location = source_location(state)
    {state, name} =
      %{state | rest: rest}
      |> move(1)
      |> collect_rest_of_name("")
    %{state | stack: [%{name: name, type: nil, source_location: location} | state.stack]}
    |> next(:in_variable_definition_type)
  end
  def in_variable_definitions(%{rest: ")" <> rest} = state) do
    variable_definitions = Enum.take_while(state.stack, fn
      %Blueprint.Document.Operation{} ->
        false
      _ ->
        true
    end)
    |> Enum.map(&struct(Blueprint.Document.VariableDefinition, &1))
    [%Blueprint.Document.Operation{} = op | stack] = Enum.drop(state.stack, length(variable_definitions))
    op = %{op | variable_definitions: variable_definitions}
    %{state | rest: rest, stack: [op | stack]}
    |> move(1)
    |> prev
  end
  skip_whitespace :in_variable_definitions
  end_rules :in_variable_definitions

  skip_whitespace :in_variable_definition_type
  def in_variable_definition_type(%{rest: ":" <> rest} = state) do
    %{state | rest: rest}
    |> move(1)
    |> in_variable_definition_type
  end
  def in_variable_definition_type(%{rest: ")" <> _} = state) do
    state
    |> prev
  end
  def in_variable_definition_type(%{rest: "!" <> rest, stack: [current_var | stack]} = state) do
    current_var = %{current_var | type: %Blueprint.TypeReference.NonNull{of_type: current_var.type} |> located(state)}
    %{state | rest: rest, stack: [current_var | stack]}
    |> move(1)
    |> in_variable_definition_type
  end
  def in_variable_definition_type(%{rest: "[" <> rest} = state) do
    %{state | rest: rest}
    |> move(1)
    |> in_variable_definition_type
  end
  def in_variable_definition_type(%{rest: "]" <> rest, stack: [current_var | stack]} = state) do
    current_var = %{current_var | type: %Blueprint.TypeReference.List{of_type: current_var.type} |> located(state)}
    %{state | rest: rest, stack: [current_var | stack]}
    |> move(1)
    |> in_variable_definition_type
  end
  def in_variable_definition_type(%{rest: <<letter::size(8), rest::binary>>, stack: [current_var | stack]} = state) when letter in @name_first do
    {state, type_name} =
      %{state | rest: rest}
      |> move(1)
      |> collect_rest_of_name(<<letter::size(8)>>)
    current_var = %{current_var | type: %Blueprint.TypeReference.Name{name: type_name} |> located(state)}
    %{state | stack: [current_var | stack]}
    |> in_variable_definition_type
  end
  end_rules :in_variable_definition_type

  #
  # FIELDS
  #

  skip_whitespace :in_fields
  def in_fields(%{rest: <<letter::size(8)>> <> rest} = state) when letter in @name_first do
    location = source_location(state)
    {state, name_or_alias} =
      %{state | rest: rest}
      |> move(1)
      |> collect_rest_of_name(<<letter::size(8)>>)
    {state, field} = build_field(state, name_or_alias, location)
    %{state | stack: [field | state.stack]}
    |> next(:in_field)
  end
  def in_fields(%{rest: "}" <> _} = state) do
    state
    |> collapse_down(Blueprint.Document.Field, :selections)
    |> prev
  end
  end_rules :in_fields

  defp build_field(%{rest: ":" <> rest} = state, alias, location) do
    {state, name} =
      %{state | rest: rest}
      |> move(1)
      |> collect_name
    {state, %Blueprint.Document.Field{name: name, alias: alias, source_location: location}}
  end
  defp build_field(state, name, location) do
    {state, %Blueprint.Document.Field{name: name, source_location: location}}
  end

  skip_whitespace :in_field
  def in_field(%{rest: "(" <> rest} = state) do
    %{state | rest: rest}
    |> move(1)
    |> next(:in_arguments)
  end
  def in_field(%{rest: "}" <> _} = state) do
    state
    |> prev
  end
  end_rules :in_field

  #
  # ARGUMENTS
  #

  skip_whitespace :in_arguments
  skip :in_arguments, [","]
  def in_arguments(%{rest: <<letter::size(8)>> <> _} = state) when letter in @name_first do
    state
    |> next(:in_argument)
  end
  def in_arguments(%{rest: ")" <> rest} = state) do
    %{state | rest: rest}
    |> move(1)
    |> collapse_down(Blueprint.Input.Argument, :arguments)
    |> prev
  end
  end_rules :in_arguments

  @end_argument_tokens [",", ")", "\n", " "]

  skip :in_argument, ["\t"]
  # ARGUMENT NAME
  def in_argument(%{rest: <<letter::size(8)>> <> rest} = state) when letter in @name_first do
    location = source_location(state)
    {state, name} =
      %{state | rest: rest}
      |> move(1)
      |> collect_rest_of_name(<<letter::size(8)>>)
    %{state | stack: [%Blueprint.Input.Argument{name: name, source_location: location} | state.stack]}
    |> in_argument
  end
  # IN VALUE
  def in_argument(%{rest: ":" <> rest} = state) do
    %{state | rest: rest}
    |> move(1)
    |> move_past_whitespace
    |> next(:in_argument_value)
  end
  # END ARGUMENT
  for token <- @end_argument_tokens do
    def in_argument(%{rest: unquote(token) <> _} = state) do
      case state do
        %{stack: [value | [%Blueprint.Input.Argument{} = arg | stack]]} ->
          %{state | stack: [%{arg | input_value: value} | stack]}
        _ ->
          state
      end
      |> prev
    end
  end
  end_rules :in_argument

  skip :in_argument_value, [":"]
  def in_argument_value(%{rest: "[" <> rest} = state) do
    list = %Blueprint.Input.List{items: []} |> located(state)
    %{state | rest: rest, stack: [list | state.stack]}
    |> move(1)
    |> in_argument_value
  end
  def in_argument_value(%{rest: "]" <> rest} = state) do
    %{state | rest: rest}
    |> move(1)
    |> collapse_down(
      fn
        %Blueprint.Input.List{} ->
          false
        _ ->
          true
      end,
      :items
    )
    |> in_argument_value
  end
  def in_argument_value(%{rest: ~s(") <> rest} = state) do
    location = source_location(state)
    {state, value} =
      %{state | rest: rest}
      |> move(1)
      |> collect_rest_of_string
    node = %Blueprint.Input.String{value: value, source_location: location}
    %{state | stack: [node | state.stack]}
    |> in_argument_value
  end
  def in_argument_value(%{rest: <<letter::size(8)>> <> rest} = state) when letter in @start_number do
    location = source_location(state)
    {state, raw_value, cast} =
      %{state | rest: rest}
      |> move(1)
      |> collect_rest_of_number_value(<<letter::size(8)>>)
    {value, _} = cast.parse(raw_value)
    node = struct(
      Module.safe_concat(Blueprint.Input, cast),
      value: value,
      source_location: location
    )
    %{state | stack: [node | state.stack]}
    |> in_argument_value
  end
  for token <- @end_argument_tokens do
    def in_argument_value(%{rest: unquote(token) <> _} = state) do
      state
      |> prev
    end
  end
  end_rules :in_argument_value

  defp apply_input_value(%{input_value: _} = host, value) do
    %{host | input_value: value}
  end
  defp apply_input_value(%{items: items} = host, value) do
    %{host | items: [value | items]}
  end

  defp collect_rest_of_number_value(state, start) do
    do_collect_rest_of_number_value(state, {start, Integer})
  end
  defp do_collect_rest_of_number_value(%{rest: <<letter::size(8)>> <> rest} = state, {result, cast}) when letter in @digits do
    %{state | rest: rest}
    |> move(1)
    |> do_collect_rest_of_number_value({result <> <<letter::size(8)>>, cast})
  end
  defp do_collect_rest_of_number_value(%{rest: "." <> rest} = state, {result, Integer}) do
    %{state | rest: rest}
    |> move(1)
    |> do_collect_rest_of_number_value({result <> ".", Float})
  end
  defp do_collect_rest_of_number_value(state, {result, cast}) do
    {state, result, cast}
  end

  # Collect a name from the current point, discarding any leading spaces
  @spec collect_name(t) :: {t, String.t}
  defp collect_name(%{rest: " " <> rest} = state) do
    %{state | rest: rest}
    |> move(1)
    |> collect_name
  end
  defp collect_name(%{rest: <<letter::size(8), rest::binary>>} = state) when letter in @name_first do
    %{state | rest: rest}
    |> move(1)
    |> do_collect_rest_of_name(<<letter::size(8)>>)
  end

  # Collect the rest of the name (after the first character)
  @spec collect_rest_of_name(t, String.t) :: {t, String.t}
  defp collect_rest_of_name(state, start) do
    do_collect_rest_of_name(state, start)
  end
  defp do_collect_rest_of_name(%{rest: <<letter::size(8), rest::binary>>} = state, acc) when letter in @name_rest do
    %{state | rest: rest}
    |> move(1)
    |> do_collect_rest_of_name(acc <> <<letter::size(8)>>)
  end
  defp do_collect_rest_of_name(state, acc) do
    {state, acc}
  end

  @escapes %{
    ?" => "\u0022",
    ?\ => "\u005C",
    ?/ => "\u002F",
    ?b => "\u0008",
    ?f => "\u000C",
    ?n => "\u000A",
    ?r => "\u000D",
    ?t => "\u0009",
  }

  # Collect the string (after the quote)
  @spec collect_rest_of_string(t) :: {t, String.t}
  defp collect_rest_of_string(state) do
    do_collect_rest_of_string(state, "")
  end

  @spec do_collect_rest_of_string(t, String.t) :: {t, String.t}
  defp do_collect_rest_of_string(%{rest: ~s(") <> rest} = state, acc) do
    state =
      %{state | rest: rest}
      |> move(1)
    {state, acc}
  end
  for {char, replacement} <- @escapes do
    defp do_collect_rest_of_string(%{rest: <<"\\", letter::size(8), rest::binary>>} = state, acc) when letter == unquote(char) do
      %{state | rest: rest}
      |> move(2)
      |> do_collect_rest_of_string(acc <> unquote(replacement))
    end
  end
  defp do_collect_rest_of_string(%{rest: <<letter::size(8)>> <> rest} = state, acc) do
    %{state | rest: rest}
    |> move(1)
    |> do_collect_rest_of_string(acc <> <<letter::size(8)>>)
  end

  @spec string_codepoint_size(integer) :: 2..4
  defp string_codepoint_size(codepoint) when codepoint < 0x800,   do: 2
  defp string_codepoint_size(codepoint) when codepoint < 0x10000, do: 3
  defp string_codepoint_size(_),                                  do: 4

  # Collect everything before a
  @spec collect_before(t, [binary]) :: {t, binary}
  defp collect_before(state, stops) do
    do_collect_before(state, stops, "")
  end
  defp do_collect_before(%{rest: <<cp::utf8, rest::binary>>} = state, stops, acc) do
    letter = <<cp::utf8>>
    case Enum.member?(stops, letter) do
      true ->
        {state, acc}
      false ->
        %{state | rest: rest}
        |> move(string_codepoint_size(cp))
        |> do_collect_before(stops, acc <> letter)
    end
  end

  @spec next(t, atom | [atom]) :: {:next, [atom] | atom, t}
  defp next(state, scope) do
    {:next, scope, state}
  end
  defp prev(state) do
    {:prev, state}
  end

end