. helpers.sh

start_test_server \
  -line 1 -file data/require-proxyauth-digest-close -end -reconnect \
  -line 4 -expect-re 'Proxy-Authorization: Digest username="testuser",realm="testrealm",nonce="948647427",uri="http://localhost:[0-9]+/",response="[0-9a-f]+",algorithm=MD5,cnonce="4a997df413e1d6b194520d3d51e1e019",nc=00000001' \
  -line 4 -file data/framed
trap "stop_test_server" EXIT
request \
  -proxy -proxy-user testuser -proxy-password testpassword \
  -get / \
  -run
