defmodule Absinthe.Parser.ResultTest do
  use ExUnit.Case, async: true

  alias Absinthe.Parser.Result

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

  defmodule Simple do
    defstruct name: nil
  end

  describe "commit/1" do

    test "returns a single item finalized" do
      assert [%Simple{}] == Result.commit([{Simple, %{}}])
    end

    test "assigns toplevel to a single value, given an :assign instruction" do
      assert [{Book, %{name: "foo"}}] == Result.commit(["foo", {:assign, :name}, {Book, %{}}])
    end

    test "assigns toplevel to a list value, reversing, given an :accumulate instruction" do
      assert [{Box, %{labels: ["study", "library"]}}] == Result.commit(["library", "study", {:accumulate, :labels}, {Box, %{}}])
    end

    test "finalizes any commited values" do
      assert [{Box, %{books: [%Book{title: "A"}, %Book{title: "B"}]}}] == Result.commit([{Book, %{title: "B"}}, {Book, %{title: "A"}}, {:accumulate, :books}, {Box, %{}}])
    end

  end

  describe "put/2" do

    test "adds a value to a result" do
      assert [{Book, %{}}] == Result.put([], {Book, %{}})
    end

  end

  describe "start/3" do

    test "adds an instruction to the result" do
      assert [{:assign, :name}, {Book, %{}}] == Result.start([{Book, %{}}], :assign, :name)
    end

  end

end
