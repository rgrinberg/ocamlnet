. helpers.sh

start_test_server \
  -break \
  -sleep 2 \
  -line 7 -file data/framed
trap "stop_test_server" EXIT
request -put-small / -run
