ERL=erl
LIBDIR=$(shell erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell)

all: src

# TODO: this is where custom include paths can be defined using -I
src: FORCE
	@$(ERL) -pa ebin -make

plt:
	@dialyzer --build_plt --output_plt .plt -q -r . -I include/

check: all
	@dialyzer --check_plt --plt .plt -q -r . -I include/\
		-I $(LIBDIR)/test_server*/include/ \
		-I $(LIBDIR)/common_test*/include/

test: test.spec src FORCE
	@run_test -pa $(PWD)/ebin -spec test.spec
	@rm variables-ct*

test.spec: test.spec.in
	@cat test.spec.in | sed -e "s,@PATH@,$(PWD)," > $(PWD)/test.spec

clean:
	rm -f ebin/*.beam
	rm -f test/*.beam
	rm test.spec

doc: FORCE
	@erl -noshell -run edoc_run application routy '"."' '[{new, true}]'

FORCE:
