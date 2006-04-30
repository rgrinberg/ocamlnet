. helpers.sh

start_test_server -line 1 -file data/status-404 -end
trap "stop_test_server" EXIT
request -get / -run
