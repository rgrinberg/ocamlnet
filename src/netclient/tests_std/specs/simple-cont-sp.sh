. helpers.sh

start_test_server -line 1 -file data/simple-cont-sp -end
trap "stop_test_server" EXIT
request -get / -run
