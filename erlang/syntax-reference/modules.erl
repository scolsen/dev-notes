%% Erlang code is organized into modules.
%% Modules are declared using the `-module(name)` declaration
%% where `name` is the name of the file the module is written in.
%% Thus, the source file defining -module(modules) must be named `modules.erl`
-module(modules).

%% Export declares which functions the module exports.
%% Exports are declared using the `-export([function/arity])` syntax.
%% You must include the function's name along with its arity. Erlang supports
%% function overloading with variant arities.
%% Functions are comma separated.
%% All functions in the module that are not exported are local to the
%% module.
-export([id/1, first/2]).

id(X) ->
  X.

first(X, Y) ->
  X.

%% To call functions from particular modules we must prefix the function name
%% with `moduleName:`
modules:id(1).
