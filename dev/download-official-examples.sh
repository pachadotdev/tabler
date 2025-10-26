#!/bin/sh

set -eu

OUT_DIR="./official-examples"
mkdir -p "$OUT_DIR"

# List of URLs to download
urls="
https://preview.tabler.io/layout-boxed.html
https://preview.tabler.io/layout-combo.html
https://preview.tabler.io/layout-condensed.html
https://preview.tabler.io/layout-fluid.html
https://preview.tabler.io/layout-fluid-vertical.html
https://preview.tabler.io/layout-horizontal.html
https://preview.tabler.io/layout-navbar-dark.html
https://preview.tabler.io/layout-navbar-overlap.html
https://preview.tabler.io/layout-navbar-sticky.html
https://preview.tabler.io/layout-vertical-right.html
https://preview.tabler.io/layout-rtl.html
https://preview.tabler.io/layout-vertical.html
https://preview.tabler.io/layout-vertical-transparent.html
"

# wget options:
# -p  : download all requisites (images, CSS, etc.)
# -k  : convert links to make pages suitable for local viewing
# -E  : adjust extension (.html) where needed
# -P  : directory prefix to save files
# -nc : skip downloads that would overwrite existing files

for url in $urls; do
	echo "Downloading: $url"
	# Use --no-clobber (-nc) to skip existing files, -P to set output dir,
	# set a browser user-agent to avoid blocking, and keep requisites/convert-links
	wget -p -k -E -nc -P "$OUT_DIR" -U "Mozilla/5.0 (X11; Linux x86_64)" "$url"
done

echo "All done. Files saved in $OUT_DIR"
