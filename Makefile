all: build run

build:
	elm make --warn examples/ClientStarter.elm --output examples/index.html
	elm make --warn examples/ServerStarter.elm --output examples/server.js

	# elm make --warn examples/demo/ClientStarter.elm --output examples/demo/chat.html
	# elm make --warn examples/demo/ServerStarter.elm --output examples/demo/server.js

run:
	node examples/server.js
