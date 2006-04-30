. helpers.sh

start_test_server \
  -line 1 -file data/require-proxyauth \
  -line 8 -expect "proxy-authorization: Basic dGVzdHVzZXI6dGVzdHBhc3N3b3Jk" \
  -line 9 -file data/framed
trap "stop_test_server" EXIT
request \
  -proxy -proxy-user testuser -proxy-password testpassword \
  -get / \
  -run
