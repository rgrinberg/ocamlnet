. helpers.sh

start_test_server \
    -line 1 -file data/framed-big \
    -line 6 -file data/framed-big \
    -line 17 -file data/framed
trap "stop_test_server" EXIT
request -get / -get / -put-big / -run

# Expected behaviour:
# - First GET request is sent to the server.
# - The client waits until the response has been arrived.
#   It does not send the other requests because of HTTP/1.0
#   compatibility.
# - Second GET request is sent to the server.
# - PUT request is sent to the server.
#   At this moment, the PUT request is being transmitted while the 
#   GET response is being received.
# - After a while the (shorter) GET response is complete. Now only
#   the sending of the PUT request is still active.
# - PUT response is received.
