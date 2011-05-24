. helpers.sh

start_test_server \
  -line 1 -file data/require-basicauth \
  -line 9 -expect "Authorization: Basic dGVzdHVzZXI6dGVzdHBhc3N3b3Jk" \
  -line 11 -file data/framed
trap "stop_test_server" EXIT
request \
  -realm testrealm -user testuser -password testpassword -basic-auth \
  -get / \
  -run
