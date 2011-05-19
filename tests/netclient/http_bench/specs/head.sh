. helpers.sh

start_test_server \
    -line 1 -file data/head \
    -line 6 -file data/head -end
trap "stop_test_server" EXIT
request -head / -head / -run
