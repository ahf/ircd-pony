## Etorrent Makefile
## Try to keep it so simple it can be run with BSD-make as well as
## GNU-make
REPO ?= ircd-pony

.PHONY: all get-deps compile clean console

# Version here is used by the test suite. It should be possible to figure
# it out automatically, but for now, hardcode.
all: compile

get-deps:
	rebar get-deps

compile:
	rebar compile

clean:
	rebar clean

