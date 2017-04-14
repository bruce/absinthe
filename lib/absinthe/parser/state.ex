defmodule Absinthe.Parser.State do
  require Logger

  defmacro __using__(_) do
    quote do
      require Logger
      Module.register_attribute(__MODULE__, :states, accumulate: true)
      Module.register_attribute(__MODULE__, :events, accumulate: true)
      import unquote(__MODULE__), only: [state: 2, enter: 2, exit: 2, match: 2]
      @generate_events [:enter, :exit, :match]
      @before_compile unquote(__MODULE__)
    end
  end

  defmacro state(name, do: block) do
    quote do
      @states unquote(name)
      @current_state unquote(name)
      unquote(block)
    end
  end

  defmacro enter(arg, do: block) do
    quote do
      @events {{@current_state, :enter}, {unquote(Macro.escape(arg)), unquote(Macro.escape(block))}}
    end
  end

  defmacro exit(arg, do: block) do
    quote do
      @events {{@current_state, :exit}, {unquote(Macro.escape(arg)), unquote(Macro.escape(block))}}
    end
  end

  defmacro match(pattern, do: block) do
    quote do
      @events {{@current_state, :match}, {unquote(Macro.escape(pattern)), unquote(Macro.escape(block))}}
    end
  end

  @doc false
  defmacro __before_compile__(env) do
    states = Module.get_attribute(env.module, :states)
    events = for state <- states, do: Absinthe.Parser.State.generate_event_functions(env, state)
    runs = for state <- states, do: Absinthe.Parser.State.generate_run_function(env, state)
    [
      events,
      runs,
      generate_dispatch
    ]
  end

  def generate_dispatch do
    quote do
      # Exit
      def dispatch(state, {:exit, %{states: [state]} = info} = returned) do
        Logger.debug("Absinthe.Parser.State dispatch state=#{state} with=#{inspect returned}")
        case __exit_state__(state, info) do
          {:ok, info} ->
            {:ok, %{info | states: []}}
          {:error, _} = err ->
            err
        end
      end
      def dispatch(state, {:exit, %{states: [state | states]} = info} = returned) do
        Logger.debug("Absinthe.Parser.State dispatch state=#{state} with=#{inspect returned}")
        case __exit_state__(state, info) do
          {:ok, info} ->
            new_state =
              states
              |> hd
            dispatch(new_state, {:match, %{info | states: states}})
          {:error, _} = err ->
            err
        end
      end
      # Match
      def dispatch(state, {:match, info} = returned) do
        Logger.debug("Absinthe.Parser.State dispatch state=#{state} with=#{inspect returned}")
        dispatch(state, __match_state__(state, info))
      end
      # Enter
      def dispatch(state, {:enter, new_state, info} = returned) do
        Logger.debug("Absinthe.Parser.State dispatch state=#{state} with=#{inspect returned}")
        dispatch(new_state, __enter_state__(new_state, %{info | states: [new_state | info.states]}))
      end
    end
  end

  def generate_run_function(env, state) do
    quote do
      def __run_state__(unquote(state), info) do
        dispatch(
          unquote(state),
          __enter_state__(unquote(state), %{info | states: [unquote(state)]})
        )
      end
    end
  end

  def generate_event_functions(env, state) do
    to_generate = Module.get_attribute(env.module, :generate_events)
    events =
      Module.get_attribute(env.module, :events)
      |> Enum.group_by(&elem(&1, 0))
    for event <- to_generate do
      case Map.get(events, {state, event}) do
        nil ->
          Absinthe.Parser.State.event_function(state, event, nil)
        provided ->
          provided
          |> Enum.reverse
          |> Enum.map(fn
            {_, config} ->
              Absinthe.Parser.State.event_function(state, event, config)
          end)
      end
    end
  end

  def event_function(state, event_name, nil) do
    func_name = "__#{event_name}_state__" |> String.to_atom
    instruction = Keyword.fetch!([enter: :match, exit: :ok, match: :match], event_name)
    quote do
      def unquote(func_name)(unquote(state), info) do
        Logger.debug("Absinthe.Parser.State stub event: event=#{unquote(event_name)} state=#{unquote(state)} arg=#{inspect(info)}")
        {unquote(instruction), info}
      end
    end
  end
  def event_function(state, event_name, {arg_ast, body_ast}) do
    func_name = "__#{event_name}_state__" |> String.to_atom
    quote do
      def unquote(func_name)(unquote(state), unquote(arg_ast) = parser_arg__) do
        Logger.debug("Absinthe.Parser.State custom event: event=#{unquote(event_name)} state=#{unquote(state)} arg=#{inspect(parser_arg__)}")
        unquote(body_ast)
      end
    end
  end

end
