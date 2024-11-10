# Pac-Man

## Playing the game
The game can be played using the command

```sh
$ cabal run
```

## Controls
```
wasd - Movement
p    - Pauses the game
esc  - Closes the game 
o    - Opens the debug overlay
```

## File structure
```
.
├───assets
│   ├───blinky
│   ├───clyde
│   ├───inky
│   ├───pacman
│   └───pinky
├───docs
│   └───img
├───scripts
│   └───data
└───src
    ├───Controller
    ├───Model
    └───View
```

## Dependencies
All dependencies are listed in the .cabal file. For completeness sake, there's an additional list here.

- Gloss
- Gloss.Juicy
- Containers
- Random
- Mtl

## Known Issues
- Resizing the window to have 0 height crashes the game.