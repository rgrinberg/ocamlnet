. helpers.sh

start_test_server \
  -break \
  -line 6 -file data/status-100 \
  -sleep 1 \
  -line 7 -file data/framed
trap "stop_test_server" EXIT
request -put-small / -run
