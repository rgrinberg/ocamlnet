. helpers.sh

start_test_server \
    -line 0 -file data/status-100 \
    -line 1 -file data/framed
trap "stop_test_server" EXIT
request -get / -run
