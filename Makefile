all:
	rm -f main.exe 2> /dev/null;dune build main.exe && mv _build/default/main.exe .

install:
	opam install psq batteries

clean:
	dune clean
