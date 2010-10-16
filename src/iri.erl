%%%-------------------------------------------------------------------
%%% File    : iri.erl
%%% Author  : Jesper Louis Andersen <jesper.louis.andersen@gmail.com>
%%% Description : IRI handling code, see RFC http://www.ietf.org/rfc/rfc3987.txt
%%%
%%% Created : 14 Oct 2010 by Jesper Louis Andersen <jesper.louis.andersen@gmail.com>
%%%-------------------------------------------------------------------
-module(iri).

%% This whole module is one big hack at the moment. It just calls through
%% to URI

%% API
-export([mk_iri/2, mk_iri/3, mk_iri/4,
	 to_iolist/1, to_string/1]).

%%====================================================================
%% API
%%====================================================================
mk_iri(S, H) ->
    uri:mk_uri(S,H,[],none).

mk_iri(S,H,Q) ->
    uri:mk_uri(S,H,Q,none).

mk_iri(S,H,Q,F) ->
    uri:mk_uri(S,H,Q,F).

to_iolist(Iri) ->
    uri:to_iolist(Iri).

to_string(IRI) ->
    uri:to_string(IRI).

