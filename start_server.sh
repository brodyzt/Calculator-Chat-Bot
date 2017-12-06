#!/bin/bash


sudo su <<EOF
make clean
eval $(opam config env)
make server
EOF


echo 2
exit 0
