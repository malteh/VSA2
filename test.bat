rm logs/*.log
erl -make
cd bin
erl -s node test
pause