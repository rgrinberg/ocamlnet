# This is an SSL-only test!

. helpers.sh

start_test_server \
    -no-ssl -line 3 -file data/status-200-connect \
    -starttls -line 4 -file data/simple -end -expect-end
trap "stop_test_server" EXIT
request -proxy -get / -run
