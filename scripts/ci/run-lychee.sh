#!/bin/sh

lychee --verbose --no-progress \
  --format markdown \
  --include-fragments \
  './**/*.md' './**/*.html' './**/*.rst'
