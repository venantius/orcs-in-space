# Orcs in space!

A stupid thing I wrote to learn Haskell and ncurses. The orcs are in space. You get the idea.

## Developing

If I'm reading this section, it's because I forgot how to set everything up.

Install Stack first.

Don't forget that for some reason ghc-mod can't be installed with anything newer than lts-9.21. This binds you to GHC 8.0.2 (Jan 2017), which isn't terrible. Maybe someday that'll get fixed.

```
# Maybe hindent as well?
stack build --copy-compiler-tool ghc-mod hoogle weeder
```

## Playing

O = An Orc. Green, stupid, and probably hungry.
S = A space marine. Trigger-happy. Avoid.
M = A door. Can you trust it? It's a door.

## Winning

What does winning mean, really?
