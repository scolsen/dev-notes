#!/bin/bash

# Loaders used by the prime-api-toolset

dot_loader () {
    echo -ne "$1 . \r"
    sleep 0.2
    echo -ne "$1 .. \r"
    sleep 0.2 
    echo -ne "$1 ... \r"
    sleep 0.2
    echo -ne "$1   \r"
    sleep 0.3
}
