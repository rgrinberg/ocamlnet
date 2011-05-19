. helpers.sh

start_test_server \
  -break \
  -line 5 -file data/status-100 \
  -sleep 1 \
  -line 5 -file data/framed
trap "stop_test_server" EXIT
request -get / -run
