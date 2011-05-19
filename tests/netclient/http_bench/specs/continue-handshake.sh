. helpers.sh

start_test_server -line 6 -file data/status-100 -line 7 -file data/framed
trap "stop_test_server" EXIT
request -handshake -put-small / -run
