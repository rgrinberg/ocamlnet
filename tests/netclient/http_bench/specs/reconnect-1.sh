. helpers.sh

# The -sleep is necessary to ensure that both GET /second and GET /third
# are sent to the server. Otherwise, the results are non-deterministic.

start_test_server \
    -line 1 -file data/framed-8bitclean \
    -sleep 1 \
    -line 6 -file data/illegal-protocol -reconnect \
    -line 1 -file data/chunked \
    -line 6 -file data/framed-8bitclean
trap "stop_test_server" EXIT
request -get /first -get /second -get /third -run
