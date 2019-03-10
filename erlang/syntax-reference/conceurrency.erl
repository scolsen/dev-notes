-module(concurrency).
-export([say_something/2]).

%% Erlang emphasizes support for concurrency. 
%% In Erlang, each execution thread is called a process when it does not share
%% data with other execution threads. When execution threads share data, they
%% are called threads proper. Erlang only supports processes.
%% Use the BIF spawn to create a process.
%% Spawn takes a module, function and arguments list as arguments.
%% The function passed to spawn must be exported by the corresponding module.
%% These examples are pulled from the quickstart doc.

say_something(What, 0) ->
  done;
say_something(What, Times) ->
  io:format("~p~n", [What]),
  say_something(What, Times - 1).

start() ->
  spawn(concurrency, say_something, [hello, 3]),
  spawn(concurrency, say_something, [goodbye, 3]).

%% The prior code will spawn two process using the say_something function.
%% The spawn function returns a process identifier (the pid of the process).
%% Process may send *messages* to each other.
%% We can pass messages to processes using the receive ... end construct.
%% Messages can be any valid erlang term.
%% Messages are received by a process input queue.
%% Receive matches the first item in the queue with each pattern in the
%% construct. If the first message fails to match any pattern, the function will
%% continue to attempt matches against the next item in the message queue. When
%% a message matches it is removed from the queue. Once we reach the end of the
%% queue the process blocks execution and waits for a new message.
%% The bang ! is used to send messages `process ! message`.
%% self() returns the PID of the process that executes self.
%% The following examples of message passing are taken from the quickstart docs.


ping(0, PID) ->
  PID ! finished,
  io:format("ping finished~n", []);
ping(N, PID) ->
  PID ! {ping, self()},
  receive 
    pong ->
      io:format("Ping received pong~n", [])
  end,
  ping(N - 1, PID).

pong() ->
  receive
    finished ->
      io:format("Pong finished~n", []);
    {ping, PID} ->
      io:format("Pong received ping~n", []),
      PID ! pong,
      pong()
  end.

start_two() ->
  Pong = spawn(concurrency, pong, []),
  spawn(concurrency, ping [3, Pong]).

%% The register function enables us to name PID so that we can reference them
%% later. This is useful as processes are not always defined dependently.
%% Register assigns some atom to the PID of a process.
%% If we are then certain of which processes should communicate, this also
%% simplifies the function heads by reducing the number of arguments as the
%% process may be referenced by name directly in the body.
ping_two(0) ->
  pong ! finished,
  io:format("ping finished~n", []);
ping_two(N) ->
  pong ! {ping, self()},
  receive 
    pong ->
      io:format("Ping received pong~n", [])
  end,
  ping(N - 1).

pong_two() ->
  receive
    finished ->
      io:format("Pong finished~n", []);
    {ping, PID} ->
      io:format("Pong received ping~n", []),
      PID ! pong,
      pong()
    end.

start_three() ->
  register(pong, spawn(concurrency, pong_two, [])),
  spawn(concurrency, ping_two, [3])

%% We can build upon Erlang's concurrency model to write distributed
%% programs--programs that may be run on multiple computers.
%% Erlang systems that communicate must use a magic cookie, usually stored in
%% the .erlang.cookie file in the Home directory. Cookie files simply need to
%% contain the same Erlang atom.
%% Each Erlang system running on a single cpu is called a node. Nodes are
%% started by passing a name argument to the erl command.
%% Node names may be passed as arguments to establish communication between
%% processes running on separate nodes. To target a registered process on a
%% particular node, we must use the form {registeredPID, NodeName}.
%% spawn/4 takes an additional node name as an argument and starts a process on
%% that node.
