. helpers.sh

start_test_server \
    -line 1 -file data/framed-big \
    -line 6 -file data/chunked-big \
    -line 11 -file data/framed-big
trap "stop_test_server" EXIT
request -get / -get / -get / -run
