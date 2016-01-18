all : ceb.native

ceb.native : ceb.ml
	corebuild -pkg str $@

