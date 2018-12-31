all : ceb.native

ceb.native : ceb.ml
	corebuild -pkg str $@

clean : 
	rm ceb.native

test : ceb.native
	./ceb.native "1,2,3" 6
