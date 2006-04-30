. helpers.sh

start_test_server \
    -line 1 -file data/framed -reconnect \
    -line 1 -file data/simple -end
trap "stop_test_server" EXIT
request -head / -get / -run
