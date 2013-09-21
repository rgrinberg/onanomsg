TRANSPORT="ipc:///tmp/pipeline.ipc"
./pipeline.native node0 $TRANSPORT & node0=$! && sleep 2
./pipeline.native node1 $TRANSPORT "Hello, World!"
./pipeline.native node1 $TRANSPORT "Goodbye."
kill $node0
