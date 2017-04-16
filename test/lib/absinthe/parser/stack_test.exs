defmodule Absinthe.Parser.StackTest do
  use ExUnit.Case, async: true

  alias Absinthe.Parser.Stack

  defmodule Book do
    @enforce_keys [:title]
    defstruct [:title]
    @type t :: %__MODULE__{
      title: String.t
    }
  end

  defmodule Box do
    defstruct [:name, :books, :labels]
    @enforce_keys [:name, :books, :labels]
    @type t :: %__MODULE__{
      name: String.t,
      labels: [String.t],
      books: [Book.t]
    }
  end

  describe "crush/1" do

    test "returns a single item unchanged" do
      assert [{Book, %{}}] == Stack.crush([{Book, %{}}])
    end

    test "assigns toplevel to a single value, given an :assign instruction" do
      assert [{Book, %{name: "foo"}}] == Stack.crush(["foo", {:assign, :name}, {Book, %{}}])
    end

    test "assigns toplevel to a list value, reversing, given an :accumulate instruction" do
      assert [{Box, %{labels: ["study", "library"]}}] == Stack.crush(["library", "study", {:accumulate, :labels}, {Box, %{}}])
    end

    test "finalizes any crushed values" do
      assert [{Box, %{books: [%Book{title: "A"}, %Book{title: "B"}]}}] == Stack.crush([{Book, %{title: "B"}}, {Book, %{title: "A"}}, {:accumulate, :books}, {Box, %{}}])
    end

  end

end
