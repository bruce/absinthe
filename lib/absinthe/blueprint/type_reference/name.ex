defmodule Absinthe.Blueprint.TypeReference.Name do

  @moduledoc false

  alias Absinthe.Phase

  @enforce_keys [:name]
  defstruct [
    :name,
    # Added by the parser
    source_location: nil,
    # Added by phases
    schema_node: nil,
    errors: []
  ]

  @type t :: %__MODULE__{
    name: String.t,
    source_location: nil | Absinthe.Blueprint.Document.SourceLocation.t,
    schema_node: Absinthe.Type.t,
    errors: [Phase.Error.t]
  }

end
