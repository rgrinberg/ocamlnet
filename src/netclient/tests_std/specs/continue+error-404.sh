. helpers.sh

start_test_server -line 7 -file data/status-404 -reconnect -line 1 -file data/simple -end
trap "stop_test_server" EXIT
request -handshake -put-lines / -get / -run
