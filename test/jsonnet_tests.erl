-module(jsonnet_tests).
-include_lib("eunit/include/eunit.hrl").

snippet_string_test() ->
    ?assertEqual("1\n", jsonnet:evaluateSnippet("1")).

snippet_number_test() ->
    ?_assertException(error, function_clause, jsonnet:evaluateSnippet(1)).

file_string_test() ->
    ?_assertException(error, function_clause, jsonnet:evaluateFile("1")).

file_number_test() ->
    ?_assertException(error, function_clause, jsonnet:evaluateFile(1)).

