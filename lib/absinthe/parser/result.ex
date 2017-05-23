defmodule Absinthe.Parser.Result do

  @type t :: [item]

  @type instruction_key :: :assign | :accumulate
  @type instruction :: {instruction_key, atom}
  @type struct_template :: {atom, map}
  @type value :: struct_template | any
  @type item :: instruction | value

  @instruction_keys [:assign, :accumulate, :append]

  @spec commit(t) :: t
  def commit([]) do
    []
  end
  def commit([item]) do
    [finalize(item)]
  end
  def commit(stack) when length(stack) > 1 do
    top =
      stack
      |> Enum.take_while(fn
        {key, _} when key in @instruction_keys ->
          false
        _ ->
         true
      end)
      rest = Enum.drop(stack, length(top))
    IO.inspect(rest)
    case rest do
      [] ->
        # No instruction found
        stack
      [instruction | [{_, %{}} = parent | ancestors]] ->
        values = Enum.map(top, &finalize/1)
        [do_commit(instruction, values, parent) | ancestors]
    end
  end

  def do_commit({:assign, key}, [value], {kind, attributes}) do
    {kind, Map.put(attributes, key, value)}
  end
  def do_commit({:accumulate, key}, values, {kind, attributes}) do
    {kind, Map.put(attributes, key, Enum.reverse(values))}
  end
  def do_commit({:append, key}, [value], {kind, attributes}) do
    current = Map.get(attributes, key, [])
    {kind, Map.put(attributes, key, [value | current])}
  end

  @instruction_keys ~w(accumulate assign append)a

  @doc false
  @spec start(t, instruction_key, atom) :: t
  def start(result, key, setting) when key in @instruction_keys do
    result
    |> put({key, setting})
  end

  @doc false
  @spec put(t, any) :: t
  def put(result, value) do
    [value | result]
  end

  @doc false
  @spec finalize(any) :: any
  def finalize({kind, %{} = attributes}) do
    struct(kind, attributes)
  end
  def finalize(value) do
    value
  end

end
