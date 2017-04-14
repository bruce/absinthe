defmodule Absinthe.Parser.StateTest do
  use ExUnit.Case

  defmodule Parser do
    use Absinthe.Parser.State, initial_state: :document
    require Logger

    def run(input) do
      __run_state__(:document, input)
    end

    state :document do
      enter shuttle  do
        {:match, %{shuttle | result: []}}
      end
      match %{rest: ""} = shuttle do
        {:exit, shuttle}
      end
      match shuttle do
        {:enter, :row, shuttle}
      end
      exit shuttle do
        {:ok, %{shuttle | result: Enum.reverse(shuttle.result)}}
      end
    end

    state :row do
      enter shuttle do
        {:enter, :cell, %{shuttle | result: [[] | shuttle.result]}}
      end
      match %{rest: "\n" <> rest} = shuttle do
        {:exit, %{shuttle | rest: rest}}
      end
      match %{rest: "," <> rest} = shuttle do
        {:enter, :cell, %{shuttle | rest: rest}}
      end
      match %{rest: <<char::size(8)>> <> rest} = shuttle do
        {:enter, :cell, shuttle}
      end
      match %{rest: ""} = shuttle do
        {:exit, shuttle}
      end
      exit %{result: [row | rows]} = shuttle do
        {:ok, %{shuttle | result: [Enum.reverse(row) | rows]}}
      end
    end

    state :cell do
      enter %{result: [row | rows]} = shuttle do
        {:match, %{shuttle | result: [["" | row] | rows]}}
      end
      match %{rest: "," <> rest} = shuttle do
        {:exit, shuttle}
      end
      match %{rest: "\n" <> rest} = shuttle do
        {:exit, shuttle}
      end
      match %{rest: <<char::size(8)>> <> rest, result: [[cell | cells] | rows]} = shuttle do
        cell = cell <> <<char::size(8)>>
        {:match, %{shuttle | rest: rest, result: [[cell | cells] | rows]}}
      end
      match %{rest: "" <> rest} = shuttle do
        {:exit, shuttle}
      end
    end

  end

  @shuttle %{rest: "name,github\nbruce,bruce\nben,benwilson512", result: nil, states: []}
  @result [~w(name github), ~w(bruce bruce), ~w(ben benwilson512)]

  describe "state/2" do

    test "creates event functions" do
      funcs = Parser.__info__(:functions)
      assert {:__enter_state__, 2} in funcs
      assert {:__exit_state__, 2} in funcs
      assert {:__match_state__, 2} in funcs
    end

    test "creates run function" do
      funcs = Parser.__info__(:functions)
      assert {:__run_state__, 2} in funcs
    end

  end

  describe "run/1" do

    test "executes correctly" do
      assert {:ok, %{@shuttle | result: @result, rest: ""}} == Parser.run(@shuttle)
    end

  end

end
