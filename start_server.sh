#!/bin/bash


sudo su <<EOF
eval $(opam config env)
make clean
make server
EOF


echo 2
exit 0
