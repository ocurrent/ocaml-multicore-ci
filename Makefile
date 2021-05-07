all:
	dune build ./service/main.exe ./client/main.exe ./web-ui/main.exe ./service/local.exe @runtest
