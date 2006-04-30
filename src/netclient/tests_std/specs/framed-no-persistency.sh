. helpers.sh

start_test_server \
  -line 1 -file data/framed -reconnect \
  -line 1 -file data/framed
trap "stop_test_server" EXIT
request -opt-inh-persistency -get / -get / -run
