. helpers.sh

start_test_server \
    -line 1 -file data/proxypersistency-1.0 \
    -line 6 -file data/proxypersistency-1.0 -expect-end
trap "stop_test_server" EXIT
request -proxy -get / -get / -run
