defmodule Absinthe.ParserTest do
  use Absinthe.Case, async: true

  describe "comment" do

    test "works" do
      assert {:ok, ["foo"], "", _, _, _} = Absinthe.Parser.__comment__(~S(#foo))
    end

  end

  describe "name" do

    test "works" do
      assert {:ok, ["Foo"], "", _, _, _} = Absinthe.Parser.__name__(~S(Foo))
    end

  end

  describe "int_value" do
    test "zero" do
      assert {:ok, [0], "", _, _, _} = Absinthe.Parser.__int_value__(~S(0))
    end
    test "positive" do
      assert {:ok, [1], "", _, _, _} = Absinthe.Parser.__int_value__(~S(1))
    end
    test "negative" do
      assert {:ok, [-1], "", _, _, _} = Absinthe.Parser.__int_value__(~S(-1))
    end

  end

  @floats %{
    "1.0" => 1.0,
    "-1.0" => -1.0,
    "12.0" => 12.0,
    "1.2e3" => 1.2e3,
    "-1.2e3" => -1.2e3,
    "-1.2e+3" => -1.2e3,
    "-1.2e-3" => -1.2e-3,
    "1e3" => 1.0e3,
    "-1e3" => -1.0e3,
    "1e-3" => 1.0e-3,
    "-1e-3" => -1.0e-3,
  }
  describe "float_value" do
    test "works" do
      for {raw, success} <- @floats do
        assert {:ok, [^success], "", _, _, _} = Absinthe.Parser.__float_value__(raw)
      end
    end
  end

  describe "string_value" do

    test "normal" do
      assert {:ok, ["Foo"], "", _, _, _} = Absinthe.Parser.__string_value__(~S("Foo"))
    end
    test "with escaped quote" do
      assert {:ok, [~S(one"two)], "", _, _, _} = Absinthe.Parser.__string_value__(~S("one\"two"))
    end
    test "with escaped tab" do
      assert {:ok, [~s(one\ttwo)], "", _, _, _} = Absinthe.Parser.__string_value__(~S("one\ttwo"))
    end
    test "with escaped newline" do
      assert {:ok, [~s(one\ntwo)], "", _, _, _} = Absinthe.Parser.__string_value__(~S("one\ntwo"))
    end
    test "with escaped crlf" do
      assert {:ok, [~s(one\r\ntwo)], "", _, _, _} = Absinthe.Parser.__string_value__(~S("one\r\ntwo"))
    end
    test "with escaped carriage return" do
      assert {:ok, [~s(one\rtwo)], "", _, _, _} = Absinthe.Parser.__string_value__(~S("one\rtwo"))
    end
    test "with escaped backspace" do
      assert {:ok, [~s(one\btwo)], "", _, _, _} = Absinthe.Parser.__string_value__(~S("one\btwo"))
    end
    test "with escaped formfeed" do
      assert {:ok, [~s(one\ftwo)], "", _, _, _} = Absinthe.Parser.__string_value__(~S("one\ftwo"))
    end
    test "with escaped unicode (uppercase)" do
      assert {:ok, [~s(one\ntwo)], "", _, _, _} = Absinthe.Parser.__string_value__(~S("one\u000Atwo"))
    end
    test "with escaped unicode (lowercase)" do
      assert {:ok, [~s(one\ntwo)], "", _, _, _} = Absinthe.Parser.__string_value__(~S("one\u000Atwo"))
    end
  end

  describe "punctuator" do
    test "with !" do
      assert {:ok, [:exclamation], "", _, _, _} = Absinthe.Parser.__punctuator__(~S(!))
    end
    test "with ..." do
      assert {:ok, [:ellipsis], "", _, _, _} = Absinthe.Parser.__punctuator__(~S(...))
    end
  end

  describe ".parse" do
    test "returns a document for a single field" do
      assert {:ok, %Absinthe.Blueprint{operations: [%Absinthe.Blueprint.Document.Operation{name: nil, type: :query, selections: [%Absinthe.Blueprint.Document.Field{name: "foo"}]}]}} = Absinthe.Parser.parse("{foo}")
    end
    test "returns a document for a multiple fields" do
      assert {:ok, %Absinthe.Blueprint{operations: [%Absinthe.Blueprint.Document.Operation{name: nil, type: :query, selections: [%Absinthe.Blueprint.Document.Field{name: "foo"}, %Absinthe.Blueprint.Document.Field{name: "bar"}]}]}} = Absinthe.Parser.parse("{foo bar}")
    end
    test "returns a document for a single field with an alias" do
      assert {:ok, %Absinthe.Blueprint{operations: [%Absinthe.Blueprint.Document.Operation{name: nil, type: :query, selections: [%Absinthe.Blueprint.Document.Field{name: "foo", alias: "mine"}]}]}} = Absinthe.Parser.parse("{mine: foo}")
    end
    test "returns a document with a query operation" do
      assert {:ok, %Absinthe.Blueprint{operations: [%Absinthe.Blueprint.Document.Operation{name: nil, type: :query, selections: [%Absinthe.Blueprint.Document.Field{name: "foo"}]}]}} = Absinthe.Parser.parse("query { foo }")
    end
    test "returns a document with a mutation operation" do
      assert {:ok, %Absinthe.Blueprint{operations: [%Absinthe.Blueprint.Document.Operation{name: nil, type: :mutation, selections: [%Absinthe.Blueprint.Document.Field{name: "foo"}]}]}} = Absinthe.Parser.parse("mutation { foo }")
    end
    test "returns a document with a subscription operation" do
      assert {:ok, %Absinthe.Blueprint{operations: [%Absinthe.Blueprint.Document.Operation{name: nil, type: :subscription, selections: [%Absinthe.Blueprint.Document.Field{name: "foo"}]}]}} = Absinthe.Parser.parse("subscription { foo }")
    end
    test "returns a document with a named operation" do
      assert {:ok, %Absinthe.Blueprint{operations: [%Absinthe.Blueprint.Document.Operation{name: "MyQuery", type: :query, selections: [%Absinthe.Blueprint.Document.Field{name: "foo"}]}]}} = Absinthe.Parser.parse("query MyQuery { foo }")
    end

  end
end
