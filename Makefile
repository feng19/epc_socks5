all: compile

###===================================================================
### build
###===================================================================
.PHONY: co compile run

co:compile
compile:
	rebar3 compile

run:
	rebar3 shell

### clean
.PHONY: clean distclean
clean:
	rebar3 clean

distclean:
	rebar3 clean -a
