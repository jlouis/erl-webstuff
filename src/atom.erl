%%%-------------------------------------------------------------------
%%% File    : atom.erl
%%% Author  : Jesper Louis Andersen <jesper.louis.andersen@gmail.com>
%%% Description : 
%%%
%%% Created : 18 Oct 2010 by Jesper Louis Andersen <jesper.louis.andersen@gmail.com>
%%%-------------------------------------------------------------------
-module(atom).
-include("uri.hrl").
%% API
-export([get_feed/1]).

%%====================================================================
%% API
%%====================================================================
-spec get_feed(string() | #uri{}) -> term().
get_feed(Uri) when is_record(Uri, uri) ->
    get_feed(uri:to_string(Uri));
get_feed(Uri) when is_list(Uri) ->
    %% Assume we get a result. No error handling yet
    {ok, "200", _Headers, Body} = ibrowse:send_req(Uri, [], get),
    xmerl_scan:string(Body).

%%--------------------------------------------------------------------
%% Function: 
%% Description:
%%--------------------------------------------------------------------

%%====================================================================
%% Internal functions
%%====================================================================
