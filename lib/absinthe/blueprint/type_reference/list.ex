defmodule Absinthe.Blueprint.TypeReference.List do

  @moduledoc false

  alias Absinthe.Blueprint

  @enforce_keys [:of_type]
  defstruct [
    :of_type,
    # Added by the parser
    source_location: nil,
    # Added by phases
    errors: []
  ]

  @type t :: %__MODULE__{
    of_type: Blueprint.TypeReference.t,
    source_location: nil | Blueprint.Document.SourceLocation.t,
    errors: [Absinthe.Phase.Error.t]
  }

end
