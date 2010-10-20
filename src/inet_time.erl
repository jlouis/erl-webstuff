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
-type date() :: {integer(), integer(), integer()}.
-type time() :: {integer(), integer(), integer(), integer()}.
-type offset() :: {'+' | '-', integer(), integer()}.

% @doc parse an RFC3339 "Internet Time" (ISO8601-subset) value.
%  Take the input string and regard it as an RFC3339 value. Parse it
%  into its components, and return these for further processing. The
%  function will return either {ok, Components} or {error, Reason} if
%  the parse is wrong or if the date can not be validated.
% @end
-spec parse(string()) ->
		   {ok, {date(), time(), offset()}} | {error, term()}.
parse(String) ->
    {ok, Tokens, _} = inet_time_scanner:string(String),
    {ok, {Date, {Time, Offset}}} = inet_time_parser_simple:parse(Tokens),
    case calendar:valid_date(Date) of
	true ->
	    case valid_time_offset(Time, Offset) of
		ok -> {ok, {Date, Time, Offset}};
		{error, Reason} ->
		    {error, {Reason, {Date, Time, Offset}}}
	    end;
	false ->
	    {error, {invalid_date, {Date, Time, Offset}}}
    end.

%%====================================================================
%% Internal functions
%%====================================================================
valid_time_offset(Time, Offset) ->
    case valid_time(Time) of
	ok -> valid_offset(Offset);
	{error, Reason} -> {error, Reason}
    end.

valid_offset({_, H, M}) when
      0 =< H,
      H =< 23,
      0 =< M,
      M =< 59 -> ok;
valid_offset(_) -> {error, invalid_offset}.

valid_time({Hour, Minute, Second, _Frac})
  when 0 =< Hour,
       Hour =< 23,
       0 =< Minute,
       Minute =< 59,
       0 =< Second,
       Second =< 60 ->
    case Second of
	60 -> {error, no_leap_second_handling};
	_  -> ok
    end;
valid_time(_Otherwise) -> {error, invalid}.



