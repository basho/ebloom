.PHONY: compile test

all: compile test

compile:
	@./rebar compile

clean:
	./rebar clean

include tools.mk
