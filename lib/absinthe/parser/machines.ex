defmodule Absinthe.Parser.Machines do
  require Logger

  alias __MODULE__

  defmacro __using__(_) do
    quote do
      require Logger
      Module.register_attribute(__MODULE__, :machines, accumulate: true)
      Module.register_attribute(__MODULE__, :events, accumulate: true)
      import unquote(__MODULE__), only: [machine: 2, enter: 3, exit: 2, match: 3, match: 2]
      @event_names [:enter, :exit, :match]
      @before_compile unquote(__MODULE__)
    end
  end

  defmacro machine(name, do: block) do
    quote do
      @machines unquote(name)
      @current_machine unquote(name)
      unquote(block)
    end
  end

  defmacro enter(arg, transition, do: block) do
    Machines.generate_event_entry(:enter, arg, transition, block)
  end

  defmacro match(arg, do: block) do
    Machines.generate_event_entry(:match, arg, :match, block)
  end
  defmacro match(arg, transition) do
    Machines.generate_event_entry(:match, arg, transition, nil)
  end
  defmacro match(arg, transition, do: block) do
    Machines.generate_event_entry(:match, arg, transition, block)
  end

  defmacro exit(arg, do: block) do
    Machines.generate_event_entry(:exit, arg, nil, block)
  end

  @spec generate_event_entry(atom, Macro.t, nil | Macro.t, nil | Macro.t) :: Macro.t
  def generate_event_entry(name, arg, transition, block) do
    quote do
      @events %{
        machine: @current_machine,
        name: unquote(name),
        transition: unquote(transition),
        arg_ast: unquote(Macro.escape(arg)),
        block_ast: unquote(Macro.escape(block))
     }
    end
  end

  @doc false
  defmacro __before_compile__(env) do
    machines = Module.get_attribute(env.module, :machines)
    events = for machine <- machines, do: Machines.generate_event_functions(env, machine)
    runs = for machine <- machines, do: Machines.generate_run_function(machine)
    [
      events,
      runs,
    ]
  end

  def generate_run_function(machine) do
    func_name = Machines.transition_function_name(machine, :enter)
    quote do
      def __run__(unquote(machine), data__) do
        unquote(func_name)(data__)
      end
    end
  end

  def generate_event_functions(env, machine) do
    to_generate = Module.get_attribute(env.module, :event_names)
    events =
      Module.get_attribute(env.module, :events)
      |> Enum.group_by(&{&1.machine, &1.name})
    for event <- to_generate do
      case Map.get(events, {machine, event}) do
        nil ->
          Machines.event_function(machine, event, nil, env)
        provided ->
          provided
          |> Enum.reverse
          |> Enum.map(&Machines.event_function(machine, event, &1, env))
      end
    end
  end

  #
  # Enter
  #

  def event_function(machine, :enter, nil, env) do
    this_func = Machines.transition_function_name(machine, :enter)
    dest_func = Machines.transition_function_name(machine, :match)
    capture = {:&, [], [{:/, [context: env.module, import: Kernel], [{dest_func, [], env.module}, 1]}]}
    quote do
      def unquote(this_func)(data__) do
        unquote(dest_func)(%{data__ | history: [unquote(capture) | data__.history]})
      end
    end
  end
  def event_function( machine, :enter, config, env) do
    this_func = Machines.transition_function_name(machine, :enter)
    dest_func = Machines.transition_function_name(machine, config.transition || :match)
    match_func = Machines.transition_function_name(machine, :match)
    capture = {:&, [], [{:/, [context: env.module, import: Kernel], [{match_func, [], env.module}, 1]}]}
    block_ast = config.block_ast || quote do: data__
    quote do
      def unquote(this_func)(unquote(config.arg_ast) = data__) do
        data__ = (unquote(block_ast))
        unquote(dest_func)(%{data__ | history: [unquote(capture) | data__.history]})
      end
    end
  end

  #
  # Match
  #

  def event_function(machine, :match, nil, _) do
    raise "The match macro must be used for machine=#{machine}"
  end
  def event_function(machine, :match, config, _) do
    this_func = Machines.transition_function_name(machine, :match)
    dest_func = Machines.transition_function_name(machine, config.transition || :match)
    block_ast = config.block_ast || quote do: data__
    quote do
      def unquote(this_func)(unquote(config.arg_ast) = data__) do
        data__ = (unquote(block_ast))
        unquote(dest_func)(data__)
      end
    end
  end

  #
  # Exit
  #

  def event_function(machine, :exit, nil, _) do
    this_func = Machines.transition_function_name(machine, :exit)
    quote do
      def unquote(this_func)(%{history: [_]} = data__) do
        %{data__ | history: []}
      end
      def unquote(this_func)(%{history: [_ | [dest_func | _] = previous_machines]} = data__) do
        dest_func.(%{data__ | history: previous_machines})
      end
    end
  end
  def event_function(machine, :exit, config, _) do
    this_func = Machines.transition_function_name(machine, :exit)
    block_ast = config.block_ast || quote do: data__
    quote do
      def unquote(this_func)(%{history: [_]} = data__) do
        unquote(config.arg_ast) = data__
        data__ = (unquote(block_ast))
        %{data__ | history: []}
      end
      def unquote(this_func)(%{history: [_ | [dest_func | _] = previous_machines]} = data__) do
        unquote(config.arg_ast) = data__
        data__ = (unquote(block_ast))
        dest_func.(%{data__ | history: previous_machines})
      end
    end
  end

  @event_names [:enter, :exit, :match]
  def transition_function_name(machine, event_name) when event_name in @event_names do
    "__#{event_name}_#{machine}__"
    |> String.to_atom
  end
  def transition_function_name(_old_machine, new_machine) do
    transition_function_name(new_machine, :enter)
  end

end
