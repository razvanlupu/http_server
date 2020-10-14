Steps to compile/run the app:
 - cd ./deps
 - git clone https://github.com/mochi/mochiweb.git
 - cd ..
 - make (compiles source files and dependencies)
 - ./start-dev.sh (starts the http server on 8080)
 
Examples:
curl -X POST -d @filename.txt http://localhost:8080/execute --header "Content-Type:application/json"

curl http://localhost:8080/test
