. helpers.sh

start_test_server \
    -line 0 -file data/framed -expect-end
trap "stop_test_server" EXIT
request -get / -run
