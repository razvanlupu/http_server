#!/bin/sh
exec erl \
    -pa ebin deps/*/ebin \
    -boot start_sasl \
    -sname http_server_dev \
    -s http_server \
    -s reloader
