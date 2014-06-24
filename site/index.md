---
title: Diagrams-cairo-raster
homepage: https://taruti.github.io/diagrams-cairo-raster
license: BSD3
---

### Installing git version of diagrams-lib (required)

The git version of
[diagrams-lib](https://github.com/diagrams/diagrams-lib) is currently
required. The relevant
[commit](https://github.com/diagrams/diagrams-lib/commit/26afbac84973a15b7fb04fd8d62043105dc0d3ca)
was merged on 2014-06-23.

### API documentation (haddock)

* [Diagrams.Backend.Cairo.Raster](https://taruti.github.io/diagrams-cairo-raster/haddock/Diagrams-Backend-Cairo-Raster.html)
* [Diagrams.Backend.Cairo.Raster.Internal](https://taruti.github.io/diagrams-cairo-raster/haddock/Diagrams-Backend-Cairo-Raster-Internal.html)
* [Diagrams.Backend.Cairo.Raster.Repa](https://taruti.github.io/diagrams-cairo-raster/haddock/Diagrams-Backend-Cairo-Raster-Repa.html)

### Plain Example

```haskell
dia :: Int -> Int -> IO (Diagram Cairo R2)
dia = cairoRaster cplus

cplus :: Int -> Int -> CairoColor
cplus x y = crgb x y (x*y)
```

### Repa Example

```haskell
dia :: Int -> Int -> IO (Diagram Cairo R2)
dia = cairoRepa (\d -> fromFunction d rplus)

rplus :: DIM2 -> CairoColor
rplus (Z :. y :. x) = crgb x y (x*y)
```
