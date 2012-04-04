all: operations

FILES=droplet.ml fountain.ml goblet.ml operations.ml

operations: $(FILES)
	@echo "Compiling..."
	ocamlc -c droplet.ml
	ocamlc -c fountain.ml
	ocamlc -c goblet.ml
	ocamlc -o operations droplet.cmo goblet.cmo fountain.cmo operations.ml

clean: 
	rm -f *.cmo *.cmi operations
