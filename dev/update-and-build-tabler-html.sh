#!/bin/bash

CORE_VERSION='1.4.0'
ICONS_VERSION='3.45.0'

mkdir -p dev

if [ ! -f "dev/tabler-core-${CORE_VERSION}.zip" ]; then
    wget "https://github.com/tabler/tabler/archive/refs/tags/@tabler/core@${CORE_VERSION}.zip" -O "dev/tabler-core-${CORE_VERSION}.zip"
fi

if [ ! -f "dev/tabler-icons-${ICONS_VERSION}.zip" ]; then
    wget "https://github.com/tabler/tabler-icons/archive/refs/tags/v${ICONS_VERSION}.zip" -O "dev/tabler-icons-${ICONS_VERSION}.zip"
fi

rm -rf "dev/tabler-core-${CORE_VERSION}"
rm -rf "dev/tabler-icons-${ICONS_VERSION}"

unzip -q "dev/tabler-core-${CORE_VERSION}.zip" -d "dev/tabler-core-${CORE_VERSION}-tmp"
mv "dev/tabler-core-${CORE_VERSION}-tmp"/*/ "dev/tabler-core-${CORE_VERSION}"
rm -rf "dev/tabler-core-${CORE_VERSION}-tmp"

unzip -q "dev/tabler-icons-${ICONS_VERSION}.zip" -d "dev/tabler-icons-${ICONS_VERSION}-tmp"
mv "dev/tabler-icons-${ICONS_VERSION}-tmp"/*/ "dev/tabler-icons-${ICONS_VERSION}"
rm -rf "dev/tabler-icons-${ICONS_VERSION}-tmp"

# sudo pacman -S pnpm

cd "dev/tabler-icons-${CORE_VERSION}"
pnpm install
pnpm run start

# If you wish to perform a one-off build without auto-refresh on any changes, you can run:
# pnpm run build

# build the docs only as a static page
(cd "dev/tabler-core-${CORE_VERSION}/docs" && pnpm install && pnpm run build)
# copy ./dist

cd "dev/tabler-icons-${ICONS_VERSION}"
pnpm install
pnpm run dev
