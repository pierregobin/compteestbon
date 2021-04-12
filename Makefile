all : _build/default/ceb.exe

_build/default/ceb.exe : ceb.ml
	dune build ceb.exe

clean : 
	rm -rf _build

test : all
	dune exec ./ceb.exe -- -c 6   2 3 -p -sol
	dune exec ./ceb.exe -- -c 10 2 3
