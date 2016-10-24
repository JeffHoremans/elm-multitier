all: build run

build:
	elm make --warn src/ClientStarter.elm --output index.html
	elm make --warn src/ServerStarter.elm --output server.js

run:
	node server.js
