Steps to compile/run the app:
	make (compiles source files and dependencies)
	./start-dev.sh (starts the http server on 8080 - you may see some PROCESS REPORTS but after a newline the shell should be there)

	if there are problems with the mochiweb dependency try manually having the project cloned in ./deps/ dir using the repo https://github.com/mochi/mochiweb

 
Examples:
	curl -X POST -d @filename.txt http://localhost:8080/execute --header "Content-Type:application/json"
	filename.txt should be at the root of the project

	the shell file output name is exec.sh

Other example:
	curl http://localhost:8080/test - prints Hello World!
