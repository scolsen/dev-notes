%% Erlang does not have an independent string data type.
%% Strings are represented as lists of Unicode characters.
%% The following list of integers, for example, may be interpreted as a string.
[97, 98, 99]. %% May be interpreted as "abc"
