. helpers.sh

start_test_server -line 7 -expect "40" -line 10 -expect "0" \
                  -line 11 -file data/simple -end
trap "stop_test_server" EXIT
request -chreq -put-small / -run
