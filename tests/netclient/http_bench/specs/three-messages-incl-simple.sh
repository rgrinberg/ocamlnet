. helpers.sh

start_test_server -line 1 -file data/framed -line 6 -file data/chunked -line 11 -file data/simple -end
trap "stop_test_server" EXIT
request -get / -get / -get / -run
