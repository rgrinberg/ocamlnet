. helpers.sh

start_test_server \
  -line 1 -file data/require-proxyauth-digest \
  -line 9 -expect-re 'Proxy-Authorization: Digest username="testuser",realm="testrealm",nonce="948647427",uri="http://localhost:[0-9]+/",response="[0-9a-f]+",algorithm=MD5,cnonce="87b7453cff04125f76e9980ef53029c6",nc=00000000' \
  -line 9 -file data/framed
trap "stop_test_server" EXIT
request \
  -proxy -proxy-user testuser -proxy-password testpassword \
  -get / \
  -run
