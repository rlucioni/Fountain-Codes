all: operations

FILES=string_droplet.ml string_fountain.ml operations.ml

operations: $(FILES)
	@echo "Compiling..."
	ocamlc -c pretty_print.ml
	ocamlc -c test_framework.ml
	ocamlc -c string_droplet.ml
	ocamlc -c string_fountain.ml
	ocamlc -o operations pretty_print.cmo test_framework.cmo string_droplet.cmo string_fountain.cmo operations.ml

clean: 
	rm -f *.cmo *.cmi operations
