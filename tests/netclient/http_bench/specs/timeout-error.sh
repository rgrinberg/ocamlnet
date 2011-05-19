. helpers.sh

start_test_server \
  -line 1 -sleep 2 -reconnect \
  -line 1 -sleep 2 -reconnect \
  -line 1 -sleep 2 -end
trap "stop_test_server" EXIT
request -opt-timeout 1 -get / -run | sed -e 's/[0-9]\+/X/g'
