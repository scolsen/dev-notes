%% **Note:** the function definitions in this file don't follow good style.
%% Using a tuple instead of two arguments in their definitions would be better
%% and reduce ambiguity. 

%% The atom datatype in Erlang represents a name with no additional information
%% or data attached.
%% Atoms must start with a lowercase letter. Atoms are not variables--you cannot
%% bind values to atoms.
-module(atoms).
-export([convert/2]).

%% Atoms are useful for dispatching across function bodies/definitions. For
%% example, the quickstart sample of inch to centimeter conversion uses an atom
%% to specify which type of calculation should be applied to the argument.
%% Atoms in function definitions and calls are in one-to-one correspondence.
convert(M, inch) ->
  N / 2.54;
convert(N, centimeter) ->
  M * 2.54;

%% We call the convert function as follows:
convert(3, inch).
convert(7, centimeter).

%% If we were to attempt to call this function with an improper atom in the
%% second argument erlang would throw a function clause exception.
