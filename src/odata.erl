%%%-------------------------------------------------------------------
%%% File    : odata.erl
%%% Author  : Jesper Louis Andersen <jesper.louis.andersen@gmail.com>
%%% Description : Various helper routines for the odata protocol
%%%
%%% Created : 12 Oct 2010 by Jesper Louis Andersen <jesper.louis.andersen@gmail.com>
%%%-------------------------------------------------------------------
-module(odata).

%% API
-export([mk_query_endpoint/3, mk_query_endpoint/4, url_escape/1]).

-type query_ops() :: none.

-record(url,
	{scheme :: http,
	 host   :: string(),
	 port   :: nil | integer(),
	 service_root :: string(),
	 resource_path :: string(),
	 query_options :: [query_ops()]}).

%%====================================================================
%% API
%%====================================================================
mk_query_endpoint(Host, SR, RP) ->
    mk_query_endpoint(Host, 80, SR, RP).

mk_query_endpoint(Host, Port, ServiceRoot, ResourcePath) ->
    #url{ scheme = http,
	  host = Host,
	  port = Port,
	  service_root = ServiceRoot,
	  resource_path = ResourcePath,
	  query_options = []}.

%% Unicode URL escaping
-spec url_escape(string()) -> iolist().
url_escape([]) -> [];
url_escape([C | R]) ->
    case test_escape(C) of
	true ->
	    Bytes = [escape(B) || B <- unicode:characters_to_list([C])],
	    [Bytes | url_escape(R)];
	false ->
	    [C | url_escape(R)]
    end.

%%====================================================================
%% Internal functions
%%====================================================================


pick(N) ->
    binary:at(<<"0123456789abcdef">>, N).

escape(B) ->
    [$%, pick(B bsr 4), pick(B band 15)].

%% Test if we should escape anything
test_escape(B) when $A =< B andalso B =< $Z -> false;
test_escape(B) when $a =< B andalso B =< $z -> false;
test_escape(B) when $0 =< B andalso B =< $0 -> false;
test_escape(B) ->
    lists:member(B, [$$, $&, $+, $,, $/, $:, $;, $=, $?, $@]).
