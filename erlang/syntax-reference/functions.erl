-module(functions).

%% Function declarations in erlang are similar in form to ML family function
%% declarations.

%% To declare a function we begin by writing its name. There is no need to
%% prefix function declarations with a keyword. 
%% Not that argument bindings/variables *must* begin with a capital letter.
id(X) ->
  X.

%% Function declarations must end with a `.`. If a function must pattern match
%% on its argument values and thus has multiple bodies, function definitions are
%% continued using a semicolon `;`.
%% Functions can match against arbitrary bindings, e.g. `X`, or concrete values
%% such as true and false.
%% Note that we must repeat the functions name when declaring subsequent bodies
%% for it, as in SML or Haskell.
negate(true) ->
  false;
negate(false) ->
  true.

%% Functions, of course, support multiple arguments.
first(X, Y) ->
  X.

%% The result of functions can be passed as arguments.
first(id(1)).

%% Function names can be overloaded with multiple arities. A function is then
%% reference by name and arity, `name/arity`.
first(X, Y, Z) ->
  X.

%% Some functions, called BIFs, are built into the Erlang language.
%% BIFs can be called without a module prefix.

%% Erlang also supports higher order functions.
%% We can bind functions to variable names using the fun keyword which defines
%% an anonymous (lambda) function.
Xf = fun(X) -> X * 2 end.

%% We can use HOF support to write the typical definition of map.
map(Fun, [First | Rest]) ->
  [Fun(First) | map(Fun, Rest)];
map(Fun, []) ->
  [].

%% To use named functions as arguments in other functions, we must prefix them
%% with the fun keyword.
List = [{1, 2}, {3, 4}, {5, 6}].
map(fun first/1, List).
