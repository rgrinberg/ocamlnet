. helpers.sh

start_test_server \
    -line 1 -file data/bad-request -break \
    -line 1 -file data/bad-request -break \
    -line 1 -file data/simple -end
trap "stop_test_server" EXIT

./test_bad_request localhost:${server_port} 2>client.out

