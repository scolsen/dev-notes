%% The match operator effectively enables use to assign values to variables. `=`
%% is the match operator.
%% The following line assigns the value 5 to the variable M.
%% Variables can only be assigned once in each scope.
%% Attempting to reassign a bound variable in the same scope results in an
%% error. 
M = 5.

%% We can destructure terms using the match operator.
%% The following statement assigns X to atom and Y to 2.
{X, Y} = {atom, 2}.
