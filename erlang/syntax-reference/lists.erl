%% Erlang supports lists and a list syntax.
%% Lists are delimited by square brackets `[` `]`
-module(lists).

%% The pipe `|` is a pattern matching mechanism on lists that is similar to
%% haskell's `:` or SML `::`
%% We can use the pipe to destructure lists or pattern match against their
%% parts. 
%% Note that pattern matching against a list that lacks the appropriate number
%% of elements to match against results in an error.
[First | Rest] = [1, 2, 3, 4, 5].
First. %% Evaluates to 1
Rest. %% Evaluates to [2,3,4,5]
%% We can match against the empty list, of course.
%% Note that the following function is not tail recursive. 
length([]) ->
  0;
length([First | Rest]) ->
  1 + length(Rest).
