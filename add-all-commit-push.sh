#!/bin/bash
./git-add.sh
git commit -a -m "$(date)"
git push origin gh-pages
