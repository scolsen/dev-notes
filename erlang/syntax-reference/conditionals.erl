%% Erlang supports the standard conditional structure. 
%% We use if ... end. syntax to specify a sequence of conditions.
%% Conditional constructs return the values specified by the succeeding
%% condition.
-module(conditionals).

%% This example is pulled from the erlang quickstart docs.
%% Each condition is ended by a continuation `;` except for the final condition.
%% If no condition succeeds Erlang will throw an exception.
if_test(A, B) ->
  if
    A == 5 ->
      io:format("A == 5~n", []),
      five;
    B == 6 ->
      io:format("B == 6~n", []),
      six;
    A == 2, B == 3 -> 
      io:format("A == 2, B == 3~n"),
      two_and_three
  end.

%% Erlang also supports a case construct. 
%% Case is similar to its common semantics in other languages and matches
%% against a specified binding.
convert(Length) ->
  case Length of 
    {centimeter, X} ->
      {inch, X / 2.54};
    {inch, Y} ->
      {centimeter, X * 2.54}
  end.
