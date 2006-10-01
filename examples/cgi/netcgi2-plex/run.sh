export LD_LIBRARY_PATH=../../../src/netsys:../../../src/netstring
exec ./netplex -conf netplex.cfg "$@"
