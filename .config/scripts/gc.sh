#!/usr/bin/bash

for path in $@
do
    echo "Clone : $path"
    git clone $path
done

