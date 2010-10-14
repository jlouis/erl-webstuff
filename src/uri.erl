%%%-------------------------------------------------------------------
%%% File    : url.erl
%%% Author  : Jesper Louis Andersen <jesper.louis.andersen@gmail.com>
%%% Description : URI handling code, as in RFC3986
%%%
%%% Created : 14 Oct 2010 by Jesper Louis Andersen <jesper.louis.andersen@gmail.com>
%%%-------------------------------------------------------------------
-module(uri).
%% API
-export([mk_uri/2, mk_uri/3, mk_uri/4,
	 uri_to_iolist/1, uri_to_string/1]).

-record(uri,
	{scheme :: atom(),
	 hier :: string(),
	 q :: [{string(), string()}],
	 fragment :: string() | none}).

%%====================================================================
%% API
%%====================================================================
mk_uri(S, H) ->
    mk_uri(S,H,[],none).

mk_uri(S,H,Q) ->
    mk_uri(S,H,Q,none).

mk_uri(S,H,Q,F) ->
    #uri{scheme = S, hier = H, q = Q, fragment = F}.

-spec uri_to_iolist(#uri{}) -> iolist().
uri_to_iolist(#uri{scheme = S, hier = H, q = Q, fragment = F}) ->
    [atom_to_list(S),
     H,
     query_to_iolist(Q),
     fragment_to_iolist(F)].

-spec uri_to_string(#uri{}) -> string().
uri_to_string(Uri) ->
    lists:flatten(uri_to_iolist(Uri)).

%%====================================================================
%% Internal functions
%%====================================================================
query_to_iolist([]) ->
    "";
query_to_iolist(L) ->
    ["?", [[percent_encode(K), "=", percent_encode(V)] || {K, V} <- L]].

fragment_to_iolist(none) ->
    "";
fragment_to_iolist(Frag) ->
    ["#", percent_encode(Frag)].

should_escape(C) when $A =< C andalso C =< $Z -> false;
should_escape(C) when $a =< C andalso C =< $z -> false;
should_escape(C) when $0 =< C andalso C =< $9 -> false;
should_escape(C) when is_integer(C) ->
    not lists:member(C, "_.-~").

hex(B) ->
    binary:sub(B, <<"0123456789abcdef">>).

hex_write(C) ->
    Upper = C bsr 4,
    Lower = C band 15,
    [hex(Upper), hex(Lower)].

percent_encode(Str) ->
    [begin
	 case should_escape(C) of
	     false -> C;
	     true when 0 =< C andalso C < 256 ->
		 ["%", hex_write(C)]
	 end
     end || C <- Str].
