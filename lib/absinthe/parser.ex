defmodule Absinthe.Parser do
  import NimbleParsec

  # CODEPOINTS

  cp_tab =
    0x0009

  cp_nl =
    0x000A

  cp_cr =
    0x000D

  cp_sp =
    0x0020

  source_character =
    utf8_char([
      cp_tab,
      cp_nl,
      cp_cr,
      cp_sp..0xFFFF
    ])

  unicode_bom =
    utf8_char([0xFEFF])

  whitespace =
    utf8_char([
      cp_tab,
      cp_sp,
    ])

  line_terminator =
    choice([
      utf8_char([cp_nl]), # newline
      utf8_char([cp_cr]) |> optional(utf8_char([cp_nl]))
    ])

  comment =
    string("#")
    |> repeat_while(source_character, {:not_line_terminator, []})

  name =
    ascii_char([?_..?_, ?A..?Z, ?a..?z])
    |> times(ascii_char([?0..?9, ?_..?_, ?A..?Z, ?a..?z]), min: 1)

  defp not_line_terminator(<<?\n, _::binary>>, context, _, _), do: {:halt, context}
  defp not_line_terminator(<<?\r, _::binary>>, context, _, _), do: {:halt, context}
  defp not_line_terminator(_, context, _, _), do: {:cont, context}

end
