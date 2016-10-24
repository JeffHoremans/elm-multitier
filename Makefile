all: build run

build:
	elm make --warn examples/ClientStarter.elm --output examples/index.html
	elm make --warn examples/ServerStarter.elm --output examples/server.js

run:
	node examples/server.js
