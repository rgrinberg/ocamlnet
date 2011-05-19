. helpers.sh

# Do first a request/response round, then shutdown in the middle of the
# next request.
# The first request is necessary to avoid that the connection is 
# closed before the client has got any response.

# Note: In most cases, this test won't force a 'broken pipe' because 
# it is unlikely that the socket is closed between the 'select' and
# the 'write' system calls in the client code.

start_test_server \
  -line 5 -file data/framed \
  -line 100 -break \
  -line 2006 -file data/framed
trap "stop_test_server" EXIT
request -get / -put-lines / -run
