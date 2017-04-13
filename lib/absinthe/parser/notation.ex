defmodule Absinthe.Parser.Notation do

  alias Absinthe.{Blueprint, Parser}

  defmacro skip(name, words) when is_list(words) do
    for word <- words do
      size = byte_size(word)
      quote do
        def unquote(name)(%{rest: unquote(word) <> rest} = state) do
          unquote(name)(
            %{state | rest: rest}
            |> move(unquote(size))
          )
        end
      end
    end
  end

  defmacro skip_spaces(name) do
    quote do
      skip(unquote(name), [" ", "\t"])
    end
  end

  defmacro skip_newlines(name) do
    quote do
      def unquote(name)(%{rest: "\n" <> rest} = state) do
        unquote(name)(
          %{state | rest: rest, cursor: move(state.cursor, 0, 1)}
        )
      end
    end
  end

  defmacro skip_whitespace(name) do
    quote do
      skip_spaces unquote(name)
      skip_newlines unquote(name)
    end
  end

  defmacro end_rules(name) do
    quote do
      def unquote(name)(state) do
        parse_error(state, unquote(name))
      end
    end
  end

  @spec collapse_down(Parser.t, ((any) -> boolean), atom) :: Parser.t
  @spec collapse_down(Parser.t, module, atom) :: Parser.t
  def collapse_down(state, criteria, key) when is_function(criteria) do
    children = Enum.take_while(state.stack, criteria)
    [parent | rest] = Enum.drop(state.stack, length(children))
    %{state | stack: [struct(parent, [{key, Enum.reverse(children)}]) | rest]}
  end
  def collapse_down(state, criteria, key) do
    collapse_down(
      state,
      fn
        %{__struct__: ^criteria} ->
          true
        _ ->
          false
      end,
      key
    )
  end

  def parse_error(%{rest: ""} = state, location) do
    {:error, "parse error at EOF (in #{location})", state}
  end
  def parse_error(%{rest: rest} = state, location) do
    {:error, "parse error at '" <> String.at(rest, 0) <> "' (in #{location})", state}
  end

  @spec source_location(Parser.t) :: Blueprint.Document.SourceLocation.t
  def source_location(state) do
    {column, line} = state.cursor
    Blueprint.Document.SourceLocation.at(line, column)
  end

  @spec located(Blueprint.node_t, Parser.t) :: Blueprint.node_t
  def located(node, state) do
    %{node | source_location: source_location(state)}
  end

  @spec move_past_whitespace(Parser.t) :: Parser.t
  def move_past_whitespace(%{rest: "\n" <> rest} = state) do
    %{state | rest: rest}
    |> move(0, 1)
  end
  for token <- [" ", "\t"] do
    def move_past_whitespace(%{rest: unquote(token) <> rest} = state) do
      %{state | rest: rest}
      |> move(1)
    end
  end
  def move_past_whitespace(state) do
    state
  end

  def move(where, x, y \\ 0)
  def move({col, row}, x, y) do
    {col + x, row + y}
  end
  def move(%{cursor: cursor} = state, x, y) do
    %{state | cursor: move(cursor, x, y)}
  end

end