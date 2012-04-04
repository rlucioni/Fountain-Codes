all: droplet fountain goblet operations

FILES= droplet.ml fountain.ml goblet.ml operations.ml

operations: $(FILES)
	@echo "Compiling..."
	ocamlc -c droplet.ml
	ocamlc -c goblet.ml
	ocamlc -c fountain.ml
	ocamlc -o operations droplet.cmo goblet.cmo fountain.cmo operations.ml

clean: 
	rm -f *.cmo *.cmi $(PROG)
