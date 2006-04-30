. helpers.sh

start_test_server \
  -line 1 -file data/require-digestauth \
  -line 9 -expect 'Authorization: Digest username="testuser",realm="testrealm",nonce="948647427",uri="/testrealm/",response="66481c52735a79763ec8ae9abbec36d7",opaque=""' \
  -line 9 -file data/framed
trap "stop_test_server" EXIT
request \
  -realm testrealm -user testuser -password testpassword -digest-auth \
  -get /testrealm/ \
  -run
