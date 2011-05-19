. helpers.sh

start_test_server -line 1 -file data/framed
trap "stop_test_server" EXIT
request -get / -run
