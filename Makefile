.PHONY: all dialyzer clean eunit run

all:
	rebar compile

dialyzer:
	rebar analyze

clean:
	rebar clean

eunit: all
	rebar eunit

run:
	erl -pz ./ebin ./deps/ibrowse/ebin
