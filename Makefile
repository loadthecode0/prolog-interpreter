

.PHONY: clean

default:
	make all clean

all:
	ocamlbuild main.native --

clean:
	ocamlbuild -clean
	rm -f *~ main.native \#*