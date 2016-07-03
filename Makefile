# Makefile 
# with another line

all : ceb.native

clean :
	rm -f ceb.native
	
ceb.native : ceb.ml
	corebuild -pkg str $@

