#!/bin/bash


make clean
eval $(opam config env)
make server 

exit 0
