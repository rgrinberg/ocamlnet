. helpers.sh

# The client discards the request after the second failed connection.
# So the first GET reports an error, and the second GET succeeds.

start_test_server \
   -line 1 -break \
   -line 1 -break \
   -line 1 -file data/framed
trap "stop_test_server" EXIT
request -get / -run -get / -run
