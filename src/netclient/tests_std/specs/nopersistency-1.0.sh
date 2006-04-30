. helpers.sh

start_test_server \
    -line 1 -file data/nopersistency-1.0 -reconnect \
    -line 1 -file data/nopersistency-1.0 
trap "stop_test_server" EXIT
request -get / -get / -run

# Because the reply is HTTP/1.0, it is expected that the second GET
# forces a new connection.
