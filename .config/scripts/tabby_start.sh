#!/bin/bash

docker run -it \
       --name tabby_local \
       --gpus all -p 8080:8080 -v $HOME/.tabby:/data \
       -e TABBY_WEBSERVER_JWT_TOKEN_SECRET=0967f805-28ee-44a1-907b-ee025968b68a \
       tabbyml/tabby \
       serve --model DeepseekCoder-6.7B --device cuda

