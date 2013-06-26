APP := cli
ERL := erl
REBAR             = `which rebar || echo ./rebar`
REBAR_SKIP_DEPS   = skip_deps=true
# configuration for dialyzer
DIALYZER_WARNINGS = -Wunmatched_returns -Werror_handling \
	-Wrace_conditions \
	-Wunderspecs \
	-Wno_undefined_callbacks
DIALYZER_APPS     = --apps kernel stdlib sasl erts ssl \
	tools mnesia inets

# Tells to make that deps is not a file/directory
.PHONY: deps

# default task
all: deps compile

# Compiles erlang sources
compile:
	@$(REBAR) compile

# Cleans all files
clean:
	@$(REBAR) clean

deps:
	@$(REBAR) get-deps

distclean:
	@$(REBAR) delete-deps

doc:
	@$(REBAR) doc skip_deps=true

eunit:
	@$(REBAR) $(REBAR_SKIP_DEPS) eunit skip_deps=true

test: clean all eunit

quick_test: compile dialyze eunit

build_plt: all
	dialyzer --build_plt --output_plt .$(APP)_dialyzer.plt \
		$(DIALYZER_APPS)

dialyze: compile
	dialyzer ebin --plt .$(APP)_dialyzer.plt \
	$(DIALYZER_WARNINGS) | \
	fgrep -v -f ./dialyzer.ignore-warnings

typer:
	typer -I deps/*/include \
		  -pa deps/lager/ebin \
		  --plt .$(APP)_dialyzer.plt \
		  src


