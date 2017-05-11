# [eventsourcing-bbl][]

This project was used as a demo to display eventsourcing design features (Friday 7th, April 2017).

This project implements a [Connect 4][] game:

* Playing a game.
* Loading a game.
* Time travelling within the game (be able to replay at any given point).

## Prerequisites

This code has been only tested on Unix-like OSes:

* Any linux distribution (on Ubuntu, you may need to install libtinfo lib).
* OSX

You need to install a Haskell build tool named [stack][]

After installing [stack][], you will have to set it up for the first time. In
project home directory enter:

```sh
$ stack setup
```

You will need to have  a running [GetEventStore][] server instance. Once unzip,
in the geteventstore directory, enter:

```s
$ ./run-node.sh --mem-db
```

## Building

```sh
$ stack build
```

## Running

```sh
$ stack exec bbl-game
```

[eventsourcing-bbl]: https://github.com/YoEight/eventsourcing-bbl

[Connect 4]:
https://en.wikipedia.org/wiki/Connect_Four

[stack]:
https://docs.haskellstack.org/en/stable/README/

[GetEventStore]:
https://geteventstore.com/downloads/
