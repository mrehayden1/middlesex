#!/bin/bash
cd assets/fonts

msdf-atlas-gen \
  -font Quattrocento-Bold.ttf \
  -and \
  -font Quattrocento-Regular.ttf \
  -and \
  -font Minecraft.ttf \
  -charset charset.txt \
  -type msdf \
  -format png \
  -imageout glyphs.png \
  -json atlas.json \
  -outerpxpadding 2
