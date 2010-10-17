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
	 to_iolist/1, to_string/1, to_uri/1,

	 should_escape/1]).

-record(ihier,
	{authority :: string() | none,
	 path      :: string() | none}).

-record(iri,
	{scheme :: atom(),
	 ihier :: #ihier{},
	 q :: [{string(), string()}],
	 fragment :: string() | none}).

%%====================================================================
%% API
%%====================================================================
mk_iri(S, H) ->
    uri:mk(S,H,[],none).

mk_iri(S,H,Q) ->
    uri:mk(S,H,Q,none).

-spec mk_iri(atom(), #ihier{}, [{string(), string()}], string() | none) ->
		    #iri{}.
mk_iri(S,H,Q,F) ->
    #iri{ scheme = S, ihier = H, q = Q, fragment = F}.

to_iolist(Iri) ->
    uri:to_iolist(Iri).

to_string(IRI) ->
    uri:to_string(IRI).

%% @doc Convert an IRI to a URI by RFC3987 Section 3.1.
%%  We assume that the IRI has already been converted into Unicode
%%  symbols (UCS) internally in Erlang. In particular, assume that the
%%  string has been <i>normalized</i>. Then we proceed to create a URI
%%  out of the IRI by converting it into UTF-8. Percent encoding then
%%  takes care of the utf_8 stream for us.
%%  @end
to_uri(#iri{scheme = S, ihier = IH, q = Q, fragment = F}) ->
    IH_E = utf_8_ihier(IH),
    Q_E  = utf_8_q(Q),
    F_E  = utf_8_f(F),
    uri:mk(S, IH_E, Q_E, F_E).

%% ----------------------------------------------------------------------
utf_8_f(Fragment) ->
    Bin = unicode:characters_to_binary(Fragment),
    binary_to_list(Bin).

utf_8_q(Q) ->
    [{utf_8_f(K), utf_8_f(V)} || {K, V} <- Q].

utf_8_ihier(#ihier{authority = A, path = P}) ->
    A_E = case A of
	      none ->
		  none;
	      S -> utf_8_f(S)
	  end,
    P_E = case P of
	      none ->
		  none;
	      Str -> utf_8_f(Str)
	  end,
    uri:mk_hier(A_E, P_E).

%% Determine if a character should be escaped in the "First run" of
%% representing an IRI. Any character greater than the UCS value of
%% U+007F is not to be mangled, as its representation is really
%% unicode already.
%%
%% This string, including U+xxxx symbols then form the basis for
%% conversion into URIs.
should_escape(C) when C > 16#7f -> false;
should_escape(_C) -> true.

