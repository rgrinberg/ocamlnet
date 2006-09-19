
export LD_LIBRARY_PATH=../../netsys

lt1: lt1.ml
	ocamlc -o lt1 -I .. unix.cma bigarray.cma ../../netsys/netsys.cma ../netshm.cma lt1.ml

run_lt1: lt1
	./lt1

clean:
	rm -f lt1 *.cmi *.cmo
