#!/bin/bash
git add tables/*/*.html data/*/*.png
git commit -a -m "$(date)"
git push origin gh-pages
