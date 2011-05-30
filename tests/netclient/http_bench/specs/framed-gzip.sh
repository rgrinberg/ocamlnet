. helpers.sh

start_test_server -line 1 -file data/framed-gzip
trap "stop_test_server" EXIT
request -gzip -response 'ABCDEFGHIJKLMNOPQRSTUVWXYZ: abcdefghijklmnopqrstuvwxyz
' -get / -run
