.PHONY: all dialyzer clean

all:
	rebar compile

dialyzer:
	rebar analyze

clean:
	rebar clean

run:
	erl -pz ./ebin ./deps/ibrowse/ebin
