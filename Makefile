all: test release

compile:
	rebar3 compile

test:
	rebar3 xref
	rebar3 dialyzer
	rebar3 eunit
	rebar3 ct
	rebar3 cover

release:
	rebar3 release

clean:
	rebar3 clean

.PHONY: release test all compile
