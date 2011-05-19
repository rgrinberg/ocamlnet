. helpers.sh

start_test_server -line 1 -file data/status-100 -file data/framed \
    -line 6 -file data/status-100 -file data/chunked \
    -line 11 -file data/status-100 -file data/framed-8bitclean
trap "stop_test_server" EXIT
request -get / -get / -get / -run
