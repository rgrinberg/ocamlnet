. helpers.sh

# Failure is reported after the second failed attempt.

start_test_server \
  -line 5 -file data/illegal-protocol -reconnect \
  -line 5 -file data/illegal-protocol
trap "stop_test_server" EXIT
request -get / -run
