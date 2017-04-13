defmodule Absinthe.Blueprint.Input.Argument do

  @moduledoc false

  alias Absinthe.Blueprint

  @enforce_keys [:name]
  defstruct [
    :name,
    # Added by parser
    source_location: nil,
    input_value: nil,
    # Added by phases
    schema_node: nil,
    value: nil, # Value converted to native elixir value
    flags: %{},
    errors: [],
  ]

  @type t :: %__MODULE__{
    name: String.t,
    input_value: nil | Blueprint.Input.Value.t,
    source_location: nil | Blueprint.Document.SourceLocation.t,
    schema_node: nil | Absinthe.Type.Argument.t,
    value: any,
    flags: Blueprint.flags_t,
    errors: [Absinthe.Phase.Error.t],
  }

  @spec value_map([t]) :: %{atom => any}
  def value_map(arguments) do
    arguments
    |> Enum.filter(fn
      %__MODULE__{schema_node: nil} ->
        false
      %__MODULE__{value: nil} ->
        false
      arg ->
        arg
    end)
    |> Map.new(&{&1.schema_node.__reference__.identifier, &1.value})
  end

end
