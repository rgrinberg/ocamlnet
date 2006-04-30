. helpers.sh

start_test_server \
  -line 1 -file data/require-basicauth \
  -line 9 -file data/require-basicauth
trap "stop_test_server" EXIT
request \
  -realm testrealm -user testuser -password testpassword -basic-auth \
  -get / \
  -run
