.SUFFIXES: .ml .mli .cmi .cmo .cma

.mli.cmi:
	ocamlc -c $<
.ml.cmo:
	ocamlc -c $<

EXEC= clean main.cmo ml
	
all: $(EXEC)

main.cmo: csv.cmi utils.cmi main.ml

ML_BINS = csv.cmo utils.cmo main.cmo
ml: $(ML_BINS)
	ocamlc -o $@ str.cma $(ML_BINS)

clean:
	rm -rf *.cm*; rm -f ml	