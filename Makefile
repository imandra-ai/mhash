
all:
	@dune build @all

clean:
	@dune clean

test:
	@dune runtest -f --no-buffer
