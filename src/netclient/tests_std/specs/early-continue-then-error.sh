. helpers.sh

start_test_server \
    -line 1 -file data/status-100 -file data/status-404 -reconnect \
    -line 1 -file data/simple -end
trap "stop_test_server" EXIT
request -put-big / -get / -run

# The second GET will only lead to a new connection if the first PUT
# request is interrupted.

