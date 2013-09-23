
all: deps compile

deps:
	rebar get-deps
	rebar update-deps

compile:
	rebar compile

test: compile
	rebar eunit apps=smullet

doc:
	rebar doc apps=smullet

clean:
	rebar clean
