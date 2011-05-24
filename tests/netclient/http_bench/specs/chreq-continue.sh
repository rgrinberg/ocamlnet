. helpers.sh

start_test_server -line 7 -file data/status-100 \
                  -line 8 -expect "40" -line 11 -expect "0" \
                  -line 12 -file data/simple -end
trap "stop_test_server" EXIT
request -chreq -handshake -put-small / -run
