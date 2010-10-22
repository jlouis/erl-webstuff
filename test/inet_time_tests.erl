-module(inet_time_tests).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").


-compile(export_all).

prop_eq0() ->
    ?FORALL({Y, M, D},
	    ?SUCHTHAT({Y, M, D},
		      {choose(1900,2100),
		       choose(0,11),
		       choose(1,31)},
		      calendar:valid_date(Y, M, D)),
	    ?FORALL({H, Min, S, SS, Dir, HO, SO},
		    {choose(0, 23),
		     choose(0, 59),
		     choose(0, 59),
		     choose(0, 1000000),
		     elements(['+', '-']),
		     choose(0, 23),
		     choose(0, 59)},
		    prop_eq({Y, M, D},
			    {H, Min, S, SS},
			    case {HO, SO} of
				{0,0} -> 'Z';
				_ -> {Dir, HO, SO}
			    end))).

prop_eq(Date, Time, Offset) ->
    Str = inet_time:to_string(Date, Time, Offset),
    {ok, {Date, Time, Offset}} = inet_time:parse(lists:flatten(Str)),
    true.

parse_test() ->
    {ok, {{1984,2,29},{23,20,50,52},{'+',8,0}}} =
	inet_time:parse("1984-02-29T23:20:50.52+8:0"),
    {error, {invalid_date,{{1984,2,30},{23,20,50,52},{'+',8,0}}}} =
	inet_time:parse("1984-02-30T23:20:50.52+8:0"),
    {ok,{{1753,9,7},{1,0,0,0},'Z'}} =
	inet_time:parse("1753-9-7T01:00:00Z"),
    done.

parse2_test() ->
    prop_eq({1984,2,29}, {23,20,50,52},{'+', 1,37}),
    ok.

qc_test() ->
    true = eqc:quickcheck(numtests(2000, prop_eq0())).
