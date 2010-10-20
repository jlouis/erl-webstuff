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
    {ok, Tokens, _} = inet_time_scanner:string(String),
    {ok, {Date, {Time, Offset}}} = inet_time_parser_simple:parse(Tokens),
    case calendar:valid_date(Date) of
	true ->
	    case valid_time(Time) of
		true -> {ok, {Date, Time, Offset}};
		false -> {error, {invalid_time, {Date, Time, Offset}}};
		leap_second -> {error, no_leap_second_handling}
	    end;
	false ->
	    {error, {invalid_date, {Date, Time, Offset}}}
    end.

%%====================================================================
%% Internal functions
%%====================================================================
valid_time({Hour, Minute, Second, _Frac})
  when 0 =< Hour,
       Hour =< 23,
       0 =< Minute,
       Minute =< 59,
       0 =< Second,
       Second =< 60 ->
    case Second of
	60 -> leap_second;
	_  -> true
    end;
valid_time(_Otherwise) -> false.



