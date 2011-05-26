. helpers.sh

start_test_server \
  -line 1 -file data/require-digestauth-close -end -reconnect \
  -line 4 -expect 'Authorization: Digest username="testuser",realm="testrealm",nonce="948647427",uri="/testrealm/",response="66481c52735a79763ec8ae9abbec36d7",algorithm=MD5,cnonce="4a997df413e1d6b194520d3d51e1e019",nc=00000001' \
  -line 4 -file data/framed
trap "stop_test_server" EXIT
request \
  -realm testrealm -user testuser -password testpassword -digest-auth \
  -get /testrealm/ \
  -run
