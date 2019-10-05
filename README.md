# AnaQRam with backend

AnaQRam is simple puzzle web game with Haskell Servant.

## AnaQRam

anagram puzzle + QR code

- anagram text is blank (`?`) before start
- display character by decode QR code
- and then resolve anagram after display all characters

## Dependency

- [Elm](http://elm-lang.org): Frontend Programming Language, alt JS
- [SQLite](https://www.sqlite.org): Backend DB

### Haskell packages

- [Servent](https://hackage.haskell.org/package/servant): Web framework for Backend
- [Persistent](https://hackage.haskell.org/package/persistent): package for database
- [elmap.hs](https://github.com/matsubara0507/elmap.hs): Generate Elm source code automatically from Haskell types

and more, refer [package.yaml](https://github.com/matsubara0507/anaqram-server/blob/master/package.yaml).

## Build

use Docker

```
$ make image --tag=anaqram
```

## Usage

use Docker


```
$ docker run --rm anaqram server --help
server [options] [input-file]
  -h  --help     Show this help text
      --version  Show version
  -v  --verbose  Enable verbose mode: verbosity level "debug"
      --migrate  Migrate SQLite
```

first, migrate SQLite

```
$ docker run --rm \
    -v path/to/.anaqram-server.yaml:/work/.anaqram-server.yaml \
    -v path/to/anaqram.sqlite:/work/anaqram.sqlite \
    anaqram server --migrate
```

run application

```
$ docker run --rm \
    -p 8080:8080 \
    -v path/to/.anaqram-server.yaml:/work/.anaqram-server.yaml \
    -v path/to/anaqram.sqlite:/work/anaqram.sqlite \
    anaqram server
```

access `localhost:8080`.
