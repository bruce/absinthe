defmodule Absinthe.Parser.Stack do

  @type t :: [item]

  @type instruction :: {:assign, atom} | {:accumulate, atom}
  @type struct_template :: {atom, map}
  @type value :: struct_template | any
  @type item :: instruction | value

  @instruction_keys [:assign, :accumulate]

  @spec crush(t) :: t
  def crush(stack) do
    top =
      stack
      |> Enum.take_while(fn
        {key, _} when key in @instruction_keys ->
          false
        _ ->
         true
      end)
    rest = Enum.drop(stack, length(top))
    case rest do
      [] ->
        # No instruction found
        stack
      [instruction | [{_, %{}} = parent | ancestors]] ->
        values = Enum.map(top, &finalize/1)
        [do_crush(instruction, values, parent) | ancestors]
    end
  end

  def do_crush({:assign, key}, [value], {kind, attributes}) do
    {kind, Map.put(attributes, key, value)}
  end
  def do_crush({:accumulate, key}, values, {kind, attributes}) do
    {kind, Map.put(attributes, key, Enum.reverse(values))}
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
