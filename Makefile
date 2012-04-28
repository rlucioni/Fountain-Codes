all: operations io_operations copier probability

operations:
	@echo "COMPILING..."
	ocamlc -c pretty_print.ml
	ocamlc -c test_framework.ml
	ocamlc -c droplet.ml
	ocamlc -c fountain.ml
	ocamlc -c goblet.ml
	ocamlc -c operations.ml
	ocamlc -o operations pretty_print.cmo test_framework.cmo droplet.cmo fountain.cmo goblet.cmo operations.cmo

io_operations:
	@echo "COMPILING..."
	ocamlc -c pretty_print.ml
	ocamlc -c test_framework.ml
	ocamlc -c droplet.ml
	ocamlc -c fountain.ml
	ocamlc -c goblet.ml
	ocamlc -c io_operations.ml
	ocamlc -o io_operations pretty_print.cmo test_framework.cmo droplet.cmo fountain.cmo goblet.cmo io_operations.cmo

copier: 
	@echo "COMPILING..."
	ocamlc -c copier.ml
	ocamlc -o copier copier.cmo

probability:
	@echo "COMPILING..."
	ocamlc -c droplet.ml
	ocamlc -c fountain.ml
	ocamlc -c goblet.ml
	ocamlc -c distribution.ml
	ocamlc -c probability.ml
	ocamlc -o probability droplet.cmo fountain.cmo goblet.cmo distribution.cmo probability.cmo

clean:
	rm -f *.cmo *.cmi operations io_operations copier probability
