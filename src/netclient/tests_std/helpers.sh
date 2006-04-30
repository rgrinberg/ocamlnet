# Some shell functions 

start_test_server () {
    rm -f server.port
    mkfifo server.port
    # Start the server in the background:
    if [ -n "$TRACED_SERVER" ]; then
	strace ./test_server -protocol "$@" 2>server.out &
    else
	./test_server -protocol "$@" 2>server.out &
    fi
    # Get the first line of the server.port file, which is
    # the port number the server listens on:
    read server_port <server.port
    #echo "Server_port: $server_port" 1>&2
}


stop_test_server () {
    if [ -f "server.pid" ]; then
        kill `cat server.pid`
    fi
}


request () {
    if [ -n "$TRACED_CLIENT" ]; then
	strace ./test_client -verbose -port "$server_port" "$@" 2>client.out
    elif [ -n "$DEBUGGED_CLIENT" ]; then
	ocamldebug -I .. ./test_client -verbose -port "$server_port" "$@"
    elif [ -n "$GDB_CLIENT" ]; then
	gdb --args ./run ./test_client -verbose -port "$server_port" "$@"
    else
	./test_client -verbose -port "$server_port" "$@" 2>client.out
    fi
}




