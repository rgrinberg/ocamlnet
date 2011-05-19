. helpers.sh

start_test_server -sleep 2 -line 7 -file data/framed
trap "stop_test_server" EXIT
request -handshake -put-small / -run
