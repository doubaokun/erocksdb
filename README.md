ERocksDB
=============

Erlang nif experiment.


### Play it

	# Compile
	rebar clean compile -v
	
	# Start it
	./start.sh

	# K-V operation
	erocksdb:put("k1", "v1").
	erocksdb:get("k1").
	erocksdb:del("k1").
	erocksdb:get("k1").

	# Write 1m k-v pairs
	erocksdb:test().