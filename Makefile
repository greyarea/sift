.PHONY: all depends compile run

all: compile

depends:
	./rebar get-deps
	./rebar update-deps

compile:
	./rebar compile skip_deps=true app=sift

run:
	erl -pz deps/*/ebin ebin \
		-boot start_sasl \
		-s sift
