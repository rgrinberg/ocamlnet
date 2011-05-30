. helpers.sh

start_test_server \
  -line 1 -file data/require-proxyauth \
  -line 10 -expect "Proxy-Authorization: Basic dGVzdHVzZXI6dGVzdHBhc3N3b3Jk" \
  -line 11 -file data/require-basicauth \
  -line 17 -expect "Authorization: Basic dGVzdHVzZXI6dGVzdHBhc3N3b3Jk" \
  -line 18 -expect "Proxy-Authorization: Basic dGVzdHVzZXI6dGVzdHBhc3N3b3Jk" \
  -line 19 -file data/framed
trap "stop_test_server" EXIT
request \
  -proxy -proxy-user testuser -proxy-password testpassword \
  -realm testrealm -user testuser -password testpassword -basic-auth \
  -get / \
  -run
