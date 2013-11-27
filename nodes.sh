rm logs/*.log
erl -make
cd bin
erl -s node start_system -setcookie vsp -sname node -name node
cd ..