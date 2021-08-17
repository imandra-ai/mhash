
all:
	@dune build @all

clean:
	@dune clean

test:
	@dune runtest -f --no-buffer

doc:
	@dune build @doc

WATCH?=@check
watch:
	@dune build $(WATCH) -w
