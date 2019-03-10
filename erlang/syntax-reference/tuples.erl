%% Erlang provides tuples and supports special tuple syntax.
-module(tuples).

%% Tuples are specified using brackets `{` `}`.
convert({centimeter, X}) ->
  {inch, X / 2.54};
convert({inch, X}) ->
  {centimeter, X * 2.54}.

%% Tuples can contain any valid Erlang term and can be of an arbitrary length.
