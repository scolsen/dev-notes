%% Erlang supports a map data type. Maps are key/value pairs (between arbitrary
%% data types).
%% The map syntax is similar to tuples and also uses braces, however, maps are
%% prefixed with an octothorp `#`. Key/value associations are denoted with `=>`.
-module(maps).

#{"k" => 1}

toMap(A,B,C,D) ->
  #{a => A, b => B, c => C, d => D}.

%% The := operator fetches values from a map and binds it to the right hand
%% side. Note that we can match strictly against the keys in the map we intend
%% to use. There's no need to handle irrelevant keys.
fetch(#{a := Aval, b := Bval}) ->
  Aval + Bval.

%% To update a map's values, we simply pass along a new map definition. 
%% := is also used to update key values.
update(Map, New) ->
  Map#{ a := New}.
