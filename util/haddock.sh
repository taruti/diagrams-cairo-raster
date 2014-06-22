#!/bin/sh

set -e

O=`mktemp -d /tmp/$USER/haddock.tmp.XXXXXXXXXX`
C=util/haddock-dark.css
SBASEF='https://github.com/taruti/diagrams-cairo-raster/blob/master/%{FILE}'
SBASEE="${SBASEF}#L%{LINE}"
OH="${O}/haddock"

[ $(basename `pwd`) == "diagrams-cairo-raster" ] || (echo "Not in correct directory!"; exit 1)

mkdir "${OH}"
echo "Work dir: ${O}"
find src -type f -name '*.hs' -print0 | xargs -0 haddock -o "${OH}" -h -U "--source-module=$SBASEF" "--source-entity=$SBASEE" -c "$C" 
git clone -b gh-pages . "${O}/repo"
mkdir -p "${O}/repo/haddock"
rm -r "${O}/repo/haddock"
mv "$OH" "${O}/repo/haddock"
(cd "${O}/repo" && git add haddock && git commit -am "Automatic haddock generation")
git fetch "${O}/repo" gh-pages:gh-pages
exit 1
rm -rf "${O}"
