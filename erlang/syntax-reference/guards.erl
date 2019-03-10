%% Erlang supports guard syntax to facilitate pattern matching. 
%% Guards are introduced using the `when` syntax.
-module(guards).

%% The following function shows an example of a guard.
%% Note that Erlang will simply use the next clause of the function if the guard
%% fails. 
max([Head | Rest], Acc) ->
  when Head > Acc ->
    max(Rest, Head);
max([Head | Rest], Acc) ->
  max(Rest, Acc).

%% User defined functions *cannot* be used in guards. Only a select number of
%% built in Erlang functions can be used in guard clauses. According to the
%% Erlang quickstart docs, this is to ensure guards cannot have side effects.
