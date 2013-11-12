./reqrep.native node0 ipc:///tmp/reqrep.ipc & node0=$!
./reqrep.native node1 ipc:///tmp/reqrep.ipc test_message & node1=$!
sleep 2
kill $node0
