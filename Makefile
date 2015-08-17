PROJECT=strftime
REBAR=bin/rebar

all: get-deps compile

build-plt:
	@dialyzer --build_plt --output_plt ~/.$(PROJECT).plt \
		--apps kernel stdlib erts

check-plt:
	@dialyzer --check_plt --plt ~/.$(PROJECT).plt

clean:
	@( $(REBAR) clean )

compile:
	@( $(REBAR) compile )

dialyze:
	@dialyzer ebin/*.beam --plt ~/.$(PROJECT).plt

doc:
	@echo "Running rebar doc..."
	@$(REBAR) skip_deps=true doc

eunit:
	@echo "Running rebar eunit..."
	@$(REBAR) skip_deps=true eunit

get-deps:
	@( $(REBAR) get-deps )

test: all eunit

.PHONY: dialyze doc eunit
