# This is an SSL-only test!

. helpers.sh

start_test_server \
    -no-ssl -line 4 -file data/require-proxy-digestauth \
    -line 8 -expect-re "Proxy-Authorization: Digest.*" \
    -line 9 -file data/status-200-connect \
    -starttls -line 10 -file data/simple -end -expect-end
trap "stop_test_server" EXIT
request -proxy -proxy-user testuser -proxy-password testpassword \
        -get / -run
