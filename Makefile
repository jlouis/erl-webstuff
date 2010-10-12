.PHONY: all dialyzer clean

all:
	rebar compile

dialyzer:
	rebar analyze

clean:
	rebar clean