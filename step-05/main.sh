#!/usr/bin/env bash
stack run -- main.grt

ld -lSystem output.o -o output
chmod +x output
./output

echo $?
