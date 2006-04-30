. helpers.sh

# This is only a weak test; you must inspect the log files to see if it has
# been passed.
# It shows: The client tolerates only five unreplied requests, and then
# waits first until some response before further requests are initiated.

start_test_server \
    -line 1 -file data/framed \
    -line 30 -file data/framed \
    -line 30 -file data/framed \
    -line 30 -file data/framed \
    -line 30 -file data/framed \
    -line 30 -file data/framed \
    -line 35 -file data/framed \
    -line 40 -file data/framed \
    -line 45 -file data/framed \
    -line 50 -file data/framed
trap "stop_test_server" EXIT
request -get / -get / -get / -get / -get / -get / -get / -get / -get / -get / -run
