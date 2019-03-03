all: 
	ocamlbuild -yaccflag -v -lib unix main.native
	mv main.native fouine

byte: 
	ocamlbuild -yaccflag -v main.byte

clean: 
	ocamlbuild -clean
