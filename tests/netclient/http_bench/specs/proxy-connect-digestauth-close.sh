# This is an SSL-only test!

. helpers.sh

start_test_server \
    -no-ssl -line 4 -file data/require-proxy-digestauth-close -end \
    -reconnect-no-ssl -reconnect \
    -line 4 -expect-re "Proxy-Authorization: Digest.*" \
    -line 5 -file data/status-200-connect \
    -starttls -line 6 -file data/simple -end -expect-end
trap "stop_test_server" EXIT
request -proxy -proxy-user testuser -proxy-password testpassword \
        -get / -run
