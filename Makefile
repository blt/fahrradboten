all: test release

compile:
	rebar3 compile

test: compile
	rebar3 xref
	rebar3 dialyzer
	rebar3 eunit
	rebar3 ct

release:
	rebar3 release


.PHONY: release test all compile
