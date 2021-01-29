# Tada

A simple Tetris in Ada.

## Build and run

Requires a GNAT toolchain and Alire (https://github.com/alire-project/alire)

Tested on Archlinux

```sh
alr build
alr run
```

## Run tests

```sh
alr run -a check
```

## How to play

Left arrow: move left

Right arrow: move right

Down arrow: move down

Up arrow: rotate

Space bar: fast down

R: restart

## Architecture

### Time

Module in charge of keeping track of time (delta time).

### Tetromino

Module representing the different tetrominos.

### Grid

Module representing the current state of the game grid. Manipulates the grid itself (lock pieces, remove lines).

### Game

Module handling a game of tetris. Consists of a grid and a current tetromino. Handles input and timing using the ``Time`` module.

### Tests

Test cases for this project

## Contracts

game.ads:23 - Make sure that the current delay is always less than the maximum delay (otherwise the current piece should have moved)

game.ads:35 - Make sure that the next piece is the one that used to be the "previewed" piece

game.ads:43 - Make sure that more than 4 lines can't be completed in one move and that the score is increased if the number of completed lines is greater than 0

game.ads:49 - Make sure that a level transition occurs when the current number of lines is greater than the numbers of line to complete in a level

game.ads:56 - Make sure that the game is still running and that the current delay is increasing

grid.ads:26 - Make sure that a piece fits before locking it and that the number of blocks in the grid increases by 4

grid.ads:31 - Make sure that the line's index is in the grid

grid.ads:35 - Make sure that the line's index is in the grid and that the line replacing the removed line is not full

grid.ads:41 - Make sure that the number of removed lines is equal to the number of full lines

## DO-178

See DO178.pdf

## Authors

Loïc Bellonnet-Mottet
Thomas Lupin
Vincent Parizet