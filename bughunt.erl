-module(bughunt).

%{Input, ExpectedOutput, ActualOutput,Comment}
-export([test/1]). 
-include_lib("~/proper/proper.hrl").
-include_lib("eunit/include/eunit.hrl").


test(I) ->
    
