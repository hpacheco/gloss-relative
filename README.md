
# Gloss Relative

This library is an extension to Gloss to reduce some of the pain of designing graphics with sizes relative to the screen dimensions.

## Usage

This package is defined as a replacement with batteries for the standard:

```
import Graphics.Gloss 
```

Just replace with the new import to get the original Gloss functionality plus the 'Frame' abstraction.

```
import Graphics.Gloss.Relative 
```

## Examples

You can play with a few examples that illustrate how the library can be used:

```
cabal run gloss-relative-checkers
cabal run gloss-relative-button
```