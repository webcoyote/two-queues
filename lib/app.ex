require :timer

defmodule App do
  use Application.Behaviour

  def start() do
    start(nil, nil)
  end

  def start(_type, _arg) do
    args = parse_cmdline

    # Start the publishers and subscribers
    num_clients = Helpers.get_int(args, :num_clients)
    # TODO: is it really necessary to create a lambda to pass a function pointer?
    #  ... what is the syntax for just passing a function?
    #  ... in erlang I think I could use fun Publisher.publisher/0
    {:ok, _pid} = :gen_server.start_link({:local, :metrics}, Metrics, [], [])
    publishers = run_workers fn -> Publisher.publisher(args) end, num_clients, []
    run_workers fn -> Subcriber.subscriber(args, publishers) end, num_clients, []

    # Wait for completion
    Metrics.reset_messages()
    receive do
      after Helpers.get_int(args, :num_seconds) * 1000 -> :ok
    end
    messages = Metrics.get_messages

    # Get the median messages per second
    messages = :lists.sort messages
    samples  = length(messages)
    median = :lists.nth(div(samples, 2) + 1, messages)
    # ^^^ one-based; not cheating to get a better sample

    IO.puts "#{median} median msg/sec"

    { :ok, self }
  end

  # Start worker tasks and get a list of their pids
  defp run_workers(_task, 0, acc) do
    acc
  end
  defp run_workers(task, count, acc) do
    pid = task.()
    run_workers(task, count - 1, [pid | acc])
  end

  # TODO: make a higher-level abstraction on OptionParser like
  # Ruby Trollop gem to avoid all this hardcoded stuff
  defp parse_cmdline do
    # Print command line
    #IO.puts "args:"
    #Enum.each System.argv, fn(arg) -> IO.puts "  #{arg}" end

    # Parse command line
    { args, _ } = OptionParser.parse(
      System.argv,
      switches: [
        redis:      :boolean,
        unbuffered: :boolean,
        quiet:      :boolean,
        verbose:    :boolean
      ]
    )

    # Verify arguments
    Enum.each args, fn(arg) ->
      { key, val } = arg
      case key do
        :redis -> :ok
        :unbuffered -> :ok
        :quiet -> :ok
        :host -> :ok
        :verbose -> :ok

        # Note the dash-to-underscore conversion by OptionParser:
        # ex: --num-clients => :"num_clients"
        :"num_clients" -> args = Dict.put args, key, parse_int(key, val)
        :"num_seconds" -> args = Dict.put args, key, parse_int(key, val)
        :"num_channels" -> args = Dict.put args, key, parse_int(key, val)
        :"message_size" -> args = Dict.put args, key, parse_int(key, val)

        key -> bad_arg :"Unknown_argument", key, ""
      end
    end

    # Set default values
    args = Dict.put_new args, :"num_clients", default_num_clients
    args = Dict.put_new args, :"num_seconds", 10
    args = Dict.put_new args, :"num_channels", 50
    args = Dict.put_new args, :"message_size", 20

    # Print arguments
    if Dict.get(args, :verbose, false) do
      Enum.each args, fn(arg) ->
        { key, val } = arg
        IO.puts "#{key}: #{val}"
      end
    end

    args
  end

  defp default_num_clients do
    [ { :processor, cpus } ] = :erlang.system_info(:cpu_topology)
    max(1, div(Kernel.length(cpus), 2))
  end

  defp parse_int(key, val) do
    if Regex.match?(%r/\A[1-9][0-9]*\z/, val) do
      binary_to_integer(val)
    else
      bad_arg(:"Invalid number", key, val)
    end
  end

  def bad_arg(error, key, val) do
    IO.puts :stderr, "#{error}: '#{key}' #{val}"
    :erlang.error error
  end

end

defmodule Publisher do

  defrecordp :pubstate, [args: nil, subscribers: nil]

  # Create a publisher to broadcast messages
  def publisher(args) do
    state = pubstate(args: args, subscribers: [])
    {:ok, pid} = :gen_server.start_link(Publisher, state, [])
    pid
  end

  def init(state) do
    {:ok, state}
  end

  def handle_info(:timeout, state) do
    args = pubstate(state, :args)
    msgsize = Helpers.get_int(args, :message_size)
    broadcast(pubstate(state, :subscribers), msgsize)
    { :noreply, state, 0 }  # timeout again immediately
  end

  def handle_call({:subscribe}, {pid,_}, state) do
    #IO.puts "P#{Kernel.inspect self}: subscribed from #{Kernel.inspect pid}"
    subscribers = [pid | pubstate(state, :subscribers)]
    { :reply, :ok, pubstate(state, subscribers: subscribers), 0 } # timeout again immediately
  end

  defp broadcast([], _msgsize) do
  end

  defp broadcast(subscribers, msgsize) do
    hd(subscribers) <- { :message, String.duplicate("a", msgsize) }
    broadcast tl(subscribers), msgsize
  end

end

defmodule Subcriber do

  defrecordp :substate, [time: nil, messages: 0, quiet: false]

  # Create a subscriber to get publisher messages
  def subscriber(args, publishers) do
    {:ok, _pid} = :gen_server.start_link(Subcriber, {args, publishers}, [])
  end

  def init({args, publishers}) do
    subscribe_to_publishers publishers
    {:ok, substate(time: :erlang.now(), quiet: Dict.get(args, :quiet)) }
  end

  defp subscribe_to_publishers ([]) do
  end
  defp subscribe_to_publishers ([pid|tail]) do
    #IO.puts "S#{Kernel.inspect self}: subscribing to #{Kernel.inspect pid}"
    :gen_server.call pid, {:subscribe}
    subscribe_to_publishers tail
  end

  def handle_info({:message, _msg}, state) do
    # Has one second elapsed?
    messages = substate(state, :messages)
    now = :erlang.now()
    delta_mms = :timer.now_diff(now, substate(state, :time))
    if delta_mms > 1000000 do
      if substate(state, :quiet) != true do
        IO.puts "#{messages} msgs/sec"
      end
      Metrics.add_messages messages
      state = substate(state, time: now, messages: 0)
    else
      state = substate(state, messages: messages + 1)
    end

    { :noreply, state }
  end

end

defmodule Metrics do

  def init(_arg) do
    {:ok, []}
  end

  def reset_messages() do
    :ok = :gen_server.call(:metrics, :reset_messages)
  end
  def get_messages() do
    {:ok, messages} = :gen_server.call(:metrics, :get_messages)
    messages
  end
  def add_messages(messages) do
    :gen_server.cast(:metrics, {:add_messages, messages})
  end

  def handle_call(:get_messages, _from, state) do
    { :reply, {:ok, state}, state }
  end
  def handle_call(:reset_messages, _from, _state) do
    { :reply, :ok, [] }
  end
  def handle_cast({:add_messages, messages}, state) do
    { :noreply, [messages | state] }
  end

end

defmodule Inspect do

  # Inspect a list
  def list([]) do
  end
  def list([hd|tl]) do
    IO.puts Kernel.inspect hd
    list tl
  end

end

defmodule Helpers do

  def get_int(dict, key) do
    # TODO: is this the easiest way to do dict[key].to_i ?
    value = Dict.get(dict, key)
    if is_binary(value) do
      value = binary_to_integer(value)
    end
    value
  end

end
