. helpers.sh

start_test_server \
    -line 1 -file data/nopersistency-close -reconnect \
    -line 1 -file data/nopersistency-close
trap "stop_test_server" EXIT
request -get / -get / -run

# Because the reply contains a 'connection: close' header, it is expected 
# that the second GET forces a new connection.
