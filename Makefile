all: test

test: compile
	./triangle 16

compile:
	for f in *.erl; do erlc -Wall +debug_info $$f; done

plt:
	dialyzer *.beam --build_plt --apps erts kernel stdlib

lint: compile
	dialyzer *.beam --quiet

clean:
	-rm *.beam
