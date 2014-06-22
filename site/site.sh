#!/bin/sh

set -e
[ $(basename `pwd`) == "diagrams-cairo-raster" ] || (echo "Not in correct directory!"; exit 1)

C="site/haddock-dark.css"
SBASEF='https://github.com/taruti/diagrams-cairo-raster/blob/master/%{FILE}'
SBASEE="${SBASEF}#L%{LINE}"
OH="site/haddock"


mkdir -p "${OH}"
rm "${OH}/*" || true
find src -type f -name '*.hs' -print0 | xargs -0 haddock -o "${OH}" -h -U "--source-module=$SBASEF" "--source-entity=$SBASEE" -c "$C" 
(cd site && runghc site.hs rebuild)

T=`mktemp -d /tmp/$USER/haddock.tmp.XXXXXXXXXX`
GHREPO="${T}/repo"

git clone -b gh-pages . "${GHREPO}"
rm -r ${GHREPO}/*
cp -a site/_site/* ${GHREPO}
(cd "${GHREPO}" && git add * && git commit -am "Automatic site generation")
git fetch "${GHREPO}" gh-pages:gh-pages
rm -rf "${T}"
