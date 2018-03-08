defmodule Absinthe.ParserTest do
  use Absinthe.Case, async: true

  describe "comment" do

    test "works" do
      assert {:ok, 'foo', "", _, _, _} = Absinthe.Parser.__comment__(~S(#foo))
    end

  end

  describe "string_value" do

    test "normal" do
      assert {:ok, 'Foo', "", _, _, _} = Absinthe.Parser.__string_value__(~S("Foo"))
    end
    test "with escaped quote" do
      assert {:ok, 'one\"two', "", _, _, _} = Absinthe.Parser.__string_value__(~S("one\"two"))
    end
    test "with escaped tab" do
      assert {:ok, 'one\ttwo', "", _, _, _} = Absinthe.Parser.__string_value__(~S("one\ttwo"))
    end
    test "with escaped newline" do
      assert {:ok, 'one\ntwo', "", _, _, _} = Absinthe.Parser.__string_value__(~S("one\ntwo"))
    end
    test "with escaped crlf" do
      assert {:ok, 'one\r\ntwo', "", _, _, _} = Absinthe.Parser.__string_value__(~S("one\r\ntwo"))
    end
    test "with escaped carriage return" do
      assert {:ok, 'one\rtwo', "", _, _, _} = Absinthe.Parser.__string_value__(~S("one\rtwo"))
    end
    test "with escaped backspace" do
      assert {:ok, 'one\btwo', "", _, _, _} = Absinthe.Parser.__string_value__(~S("one\btwo"))
    end
    test "with escaped formfeed" do
      assert {:ok, 'one\ftwo', "", _, _, _} = Absinthe.Parser.__string_value__(~S("one\ftwo"))
    end
    test "with escaped unicode (uppercase)" do
      assert {:ok, [?o, ?n, ?e, "\n", ?t, ?w, ?o], "", _, _, _} = Absinthe.Parser.__string_value__(~S("one\u000Atwo"))
    end
    test "with escaped unicode (lowercase)" do
      assert {:ok, [?o, ?n, ?e, "\n", ?t, ?w, ?o], "", _, _, _} = Absinthe.Parser.__string_value__(~S("one\u000Atwo"))
    end
  end

end
