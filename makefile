MODULES=model view controller
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml) runcontroller.ml
MLIS=$(MODULES:=.mli)
TEST=test.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind
PKGS=unix,oUnit,str,qcheck,csv,ANSITerminal
default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS) runcontroller.ml

zip: 
	zip src.zip *.ml* _tags makefile INSTALL.txt LOC.txt

test: 
	$(OCAMLBUILD) $(MLS) && $(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST)

docs: docs-public docs-private
	
docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package $(PKGS) \
	        -html -stars -d doc.public $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package $(PKGS) \
	        -html -stars -d doc.private \
	        -inv-merge-ml-mli -m A -hide-warnings $(MLIS) $(MLS)

run: build
	$(OCAMLBUILD) runcontroller.byte && ./runcontroller.byte

clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private report src.zip bisect*.out *.byte
