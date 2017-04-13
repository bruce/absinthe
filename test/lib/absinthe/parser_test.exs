defmodule Absinthe.ParserTest do
  use Absinthe.Case, async: true

  alias Absinthe.Blueprint

  describe ".parse" do

    describe "on an empty document" do

      it "returns a Blueprint.t" do
        assert {:ok, %Blueprint{}, _} = parse("")
      end

    end

    describe "on an empty anonymous document" do

      it "returns a Blueprint.t with a single query operation" do
        assert {
          :ok,
          %Blueprint{operations: [%Blueprint.Document.Operation{type: :query, source_location: %{column: 1, line: 1}}]},
          %{cursor: {3, 1}}
        } = parse("{}")
      end

      it "even with spacing, returns a Blueprint.t with a single query operation" do
        assert {
          :ok,
          %Blueprint{operations: [%Blueprint.Document.Operation{type: :query, source_location: %{column: 2, line: 1}}]},
          %{cursor: {7, 2}}
        } = parse(" { \n } ")
      end

    end

    describe "on query" do

      it "returns a Blueprint.t with a single query operation" do
        assert {
          :ok,
          %Blueprint{operations: [%Blueprint.Document.Operation{type: :query, source_location: %{column: 1, line: 1}}]},
          %{cursor: {9, 1}}
        } = parse("query {}")
      end

      it "named, returns a Blueprint.t with a single query operation" do
        assert {
          :ok,
          %Blueprint{operations: [%Blueprint.Document.Operation{type: :query, name: "MyQuery", source_location: %{column: 1, line: 1}}]},
          %{cursor: {17, 1}}
        } = parse("query MyQuery {}")
      end

      it "with a simple field, returns the correct Blueprint.t" do
        assert {
          :ok,
          %Blueprint{operations: [%Blueprint.Document.Operation{type: :query, source_location: %{column: 1, line: 1}, selections: [
            %Blueprint.Document.Field{name: "foo", source_location: %{column: 3, line: 1}}
          ]}]},
          %{cursor: {8, 1}}
        } = parse("{ foo }")
      end

      it "with a string argument, returns the correct Blueprint.t" do
        assert {
          :ok,
          %Blueprint{operations: [%Blueprint.Document.Operation{type: :query, source_location: %{column: 1, line: 1}, selections: [
            %Blueprint.Document.Field{name: "foo", source_location: %{column: 3, line: 1}, arguments: [
              %Blueprint.Input.Argument{
                name: "bar",
                input_value: %Blueprint.Input.String{
                  value: "baz",
                  source_location: %{column: 12, line: 1}
                },
                source_location: %{column: 7, line: 1}
              }
            ]}
          ]}]},
          %{cursor: {20, 1}}
        } = parse(~s<{ foo(bar: "baz") }>)
      end

      it "with a string argument with escaping, returns the correct Blueprint.t" do
        assert {
          :ok,
          %Blueprint{operations: [%Blueprint.Document.Operation{type: :query, selections: [
            %Blueprint.Document.Field{name: "foo", arguments: [
              %Blueprint.Input.Argument{
                name: "bar",
                input_value: %Blueprint.Input.String{
                  value: ~s(baz"\b\f\n\r\tspam)
                }
              }
            ]}
          ]}]},
          %{cursor: {36, 1}}
        } = parse(~S<{ foo(bar: "baz\"\b\f\n\r\tspam") }>)
      end

      it "with an integer argument, returns the correct Blueprint.t" do
        assert {
          :ok,
          %Blueprint{operations: [%Blueprint.Document.Operation{type: :query, selections: [
            %Blueprint.Document.Field{name: "foo", arguments: [
              %Blueprint.Input.Argument{
                name: "bar",
                input_value: %Blueprint.Input.Integer{
                  value: 1
                }
              }
            ]}
          ]}]},
          %{cursor: {16, 1}}
        } = parse("{ foo(bar: 1) }")
      end

      for separator <- [", ", " ", "\n"] do
        it "with multiple integer arguments, separated by #{inspect separator} returns the correct Blueprint.t" do
          expected_cursor =
            case unquote(separator) do
              "\n" ->
                {22, 2}
              other ->
                {22 + byte_size(other), 1}
            end
          assert {
            :ok,
            %Blueprint{operations: [%Blueprint.Document.Operation{type: :query, selections: [
              %Blueprint.Document.Field{name: "foo", arguments: [
                %Blueprint.Input.Argument{
                  name: "bar",
                  input_value: %Blueprint.Input.Integer{
                    value: 1
                  }
                },
                %Blueprint.Input.Argument{
                  name: "baz",
                  input_value: %Blueprint.Input.Integer{
                    value: 2
                  }
                }
              ]}

            ]}]},
            %{cursor: ^expected_cursor}
          } = parse("{ foo(bar: 1#{unquote(separator)}baz: 2) }")
        end
      end

      it "with a negative integer argument, returns the correct Blueprint.t" do
        assert {
          :ok,
          %Blueprint{operations: [%Blueprint.Document.Operation{type: :query, selections: [
            %Blueprint.Document.Field{name: "foo", arguments: [
              %Blueprint.Input.Argument{
                name: "bar",
                input_value: %Blueprint.Input.Integer{
                  value: -1
                }
              }
            ]}
          ]}]},
          %{cursor: {17, 1}}
        } = parse("{ foo(bar: -1) }")
      end

      it "with a float argument, returns the correct Blueprint.t" do
        assert {
          :ok,
          %Blueprint{operations: [%Blueprint.Document.Operation{type: :query, selections: [
            %Blueprint.Document.Field{name: "foo", arguments: [
              %Blueprint.Input.Argument{
                name: "bar",
                input_value: %Blueprint.Input.Float{
                  value: 1.1
                }
              }
            ]}
          ]}]},
          %{cursor: {18, 1}}
        } = parse("{ foo(bar: 1.1) }")
      end

      it "with a negative float argument, returns the correct Blueprint.t" do
        assert {
          :ok,
          %Blueprint{operations: [%Blueprint.Document.Operation{type: :query, selections: [
            %Blueprint.Document.Field{name: "foo", arguments: [
              %Blueprint.Input.Argument{
                name: "bar",
                input_value: %Blueprint.Input.Float{
                  value: -1.1
                }
              }
            ]}
          ]}]},
          %{cursor: {19, 1}}
        } = parse("{ foo(bar: -1.1) }")
      end

      it "with a simple field and an alias, returns the correct Blueprint.t" do
        assert {
          :ok,
          %Blueprint{operations: [%Blueprint.Document.Operation{type: :query, selections: [
            %Blueprint.Document.Field{name: "foo", alias: "bar"}
          ]}]},
          %{cursor: {13, 1}}
        } = parse("{ bar: foo }")
      end

     it "with variable definitions, returns a Blueprint.t with a single query operation" do
        assert {
          :ok,
          %Blueprint{
            operations: [
              %Blueprint.Document.Operation{
                type: :query,
                variable_definitions: [
                  %Blueprint.Document.VariableDefinition{
                    name: "foo",
                    type: %Blueprint.TypeReference.NonNull{
                      of_type: %Blueprint.TypeReference.List{
                        of_type: %Blueprint.TypeReference.NonNull{
                          of_type: %Blueprint.TypeReference.Name{
                            name: "String"
                          }
                        }
                      }
                    }
                  }
                ]
              }
            ]
          },
          %{cursor: {28, 1}}
        } = parse("query ($foo: [String!]!) {}")
      end

      it "with a list of integers argument, returns the correct Blueprint.t" do
        assert {
          :ok,
          %Blueprint{operations: [%Blueprint.Document.Operation{type: :query, selections: [
            %Blueprint.Document.Field{name: "foo", arguments: [
              %Blueprint.Input.Argument{
                name: "bar",
                input_value: %Blueprint.Input.List{
                  items: [
                    %Blueprint.Input.Integer{value: 1}
                  ]
                }
              }
            ]}
          ]}]},
          %{cursor: {18, 1}}
        } = parse("{ foo(bar: [1]) }")
      end

    end

    describe "on mutation" do

      it "anonymous, returns a Blueprint.t with a single mutation operation" do
        assert {
          :ok,
          %Blueprint{operations: [%Blueprint.Document.Operation{type: :mutation}]},
          %{cursor: {12, 1}}
        } = parse("mutation {}")
      end

      it "named, returns a Blueprint.t with a single mutation operation" do
        assert {
          :ok,
          %Blueprint{operations: [%Blueprint.Document.Operation{type: :mutation, name: "MyMutation"}]},
          %{cursor: {23, 1}}
        } = parse("mutation MyMutation {}")
      end

    end

    describe "on subscription" do

      it "anonymous, returns a Blueprint.t with a single subscription operation" do
        assert {
          :ok,
          %Blueprint{operations: [%Blueprint.Document.Operation{type: :subscription}]},
          %{cursor: {16, 1}}
        } = parse("subscription {}")
      end

      it "named, returns a Blueprint.t with a single subscription operation" do
        assert {
          :ok,
          %Blueprint{operations: [%Blueprint.Document.Operation{type: :subscription, name: "MySubscription"}]},
          %{cursor: {31, 1}}
        } = parse("subscription MySubscription {}")
      end

    end

  end

  defp parse(text) do
    Absinthe.Parser.run(text)
  end

end