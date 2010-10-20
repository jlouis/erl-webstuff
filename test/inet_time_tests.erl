-module(inet_time_tests).

-include_lib("eunit/include/eunit.hrl").

parse_test() ->
    {ok, {{1984,2,29},{23,20,50,52},{'+',8,0}}} =
	inet_time:parse("1984-02-29T23:20:50.52+8:0"),
    {error, {invalid_date,{{1984,2,30},{23,20,50,52},{'+',8,0}}}} =
	inet_time:parse("1984-02-30T23:20:50.52+8:0"),
    {ok,{{1753,9,7},{1,0,0,0},'Z'}} =
	inet_time:parse("1753-9-7T01:00:00Z"),
    {error, {invalid_date,{{1752,9,7},{1,0,0,0},'Z'}}} =
	inet_time:parse("1752-9-7T01:00:00Z"),
    done.
