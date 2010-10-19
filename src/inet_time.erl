%%%-------------------------------------------------------------------
%%% File    : inet_time.erl
%%% Author  : Jesper Louis Andersen <jesper.louis.andersen@gmail.com>
%%% Description : "Internet Time", RFC 3339
%%%
%%% Created : 19 Oct 2010 by Jesper Louis Andersen <jesper.louis.andersen@gmail.com>
%%%-------------------------------------------------------------------
-module(inet_time).

%% API
-export([parse/1]).

%%====================================================================
%% API
%%====================================================================
parse(String) ->
    try	parse_iso_date_time(String) of
	{Ast, []} -> {ok, Ast};
	{Ast, NoFull} -> {error, {spurious_trailing_chars, Ast, NoFull}}
    catch
	{no_parse, Reason} ->
	    {error, Reason}
    end.



%%====================================================================
%% Internal functions
%%====================================================================
parse_iso_date_time(S) ->
    {Date, R1} = parse_iso_date(S),
    {_, R2}    = expect("T", R1),
    {Time, []} = parse_iso_time(R2),
    {iso_8601, Date, Time}.

parse_interval_2(Limit, [D1, D2 | S])
  when $0 =< D1, D1 =< $9, $0 =< D2, D2 =< $9 ->
    N = list_to_integer([D1, D2]),
    if
	0 =< N andalso N =< Limit ->
	    {N, S};
	true ->
	    throw({no_parse, {invalid_integer_limit, N, Limit}})
    end;
parse_interval_2(Limit, _) ->
    throw({no_parse, {expected_integer_limit, Limit}}).

parse_time_hour(S) ->
    parse_interval_2(24, S).

parse_time_minute(S) ->
    parse_interval_2(59, S).

%% Here we just parse the minutes, we don't do anything specific with leap seconds, but accept any leap second value.
parse_time_second(S) ->
    parse_interval_2(60, S).

parse_iso_date(S) ->
    {todo, S}.

parse_iso_time(S) ->
    {todo, S}.

expect(Prefix, S) ->
    case lists:prefix(Prefix, S) of
	true ->
	    lists:split(length(Prefix), S);
	false ->
	    throw({no_parse, {wrong_prefix, Prefix}})
    end.


