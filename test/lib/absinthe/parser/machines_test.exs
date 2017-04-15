defmodule Absinthe.Parser.MachinesTest do
  use ExUnit.Case

  defmodule Parser do
    use Absinthe.Parser.Machines

    def run(input) do
      __run__(:document, input)
    end

    machine :document do
      enter data, :match  do
        %{data | result: []}
      end
      match %{input: ""}, :exit
      match _, :row
      exit data do
        %{data | result: Enum.reverse(data.result)}
      end
    end

    machine :row do
      enter data, :cell do
        %{data | result: [[] | data.result]}
      end
      match %{input: "\n" <> rest} = data, :exit do
        %{data | input: rest}
      end
      match %{input: "," <> rest} = data, :cell do
        %{data | input: rest}
      end
      match %{input: <<_::size(8)>> <> _}, :cell
      match _, :exit
      exit %{result: [row | rows]} = data do
        %{data | result: [Enum.reverse(row) | rows]}
      end
    end

    machine :cell do
      enter %{result: [row | rows]} = data, :match do
        %{data | result: [["" | row] | rows]}
      end
      match %{input: "," <> _}, :exit
      match %{input: "\n" <> _}, :exit
      match %{input: <<char::size(8)>> <> rest, result: [[cell | cells] | rows]} = data, :match do
        cell = cell <> <<char::size(8)>>
        %{data | input: rest, result: [[cell | cells] | rows]}
      end
      match %{input: "" <> _}, :exit
    end

  end

  @data %{input: "name,github\nbruce,bruce\nben,benwilson512", result: nil, history: []}
  @result [~w(name github), ~w(bruce bruce), ~w(ben benwilson512)]

  describe "machine/2" do

    test "creates event functions" do
      funcs = Parser.__info__(:functions)
      assert {:__enter_document__, 1} in funcs
      assert {:__enter_row__, 1} in funcs
      assert {:__enter_cell__, 1} in funcs
      assert {:__exit_document__, 1} in funcs
      assert {:__exit_row__, 1} in funcs
      assert {:__exit_cell__, 1} in funcs
      assert {:__match_document__, 1} in funcs
      assert {:__match_row__, 1} in funcs
      assert {:__match_cell__, 1} in funcs
    end

    test "creates run function" do
      funcs = Parser.__info__(:functions)
      assert {:__run__, 2} in funcs
    end

  end

  describe "run/1" do

    test "executes correctly" do
      assert %{@data | result: @result, input: ""} == Parser.run(@data)
    end

  end

end
