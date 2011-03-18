#!/bin/sh
cd `dirname $0`
exec erl -sname erlnode -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -s reloader -s httptimer
