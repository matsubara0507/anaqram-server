# anaqram-server
Score Ranking Server for [AnaQRam](https://github.com/matsubara0507/AnaQRam) with Haskell Servant

## Dependency

- [Elm](http://elm-lang.org): Frontend Programming Language, alt JS
- [PostgreSQL](https://www.postgresql.org/): Backend DB

if use [MySQL](https://www.mysql.com) alt PostgreSQL, refer [`mysql`](https://github.com/matsubara0507/anaqram-server/tree/mysql) branch.

### Haskell packages

- [Servent](https://hackage.haskell.org/package/servant): Web framework for Backend 
- [Persistent](https://hackage.haskell.org/package/persistent): package for database
- [elm-export](https://hackage.haskell.org/package/elm-export): Generate Elm source code automatically from Haskell types
    - [servant-elm](https://hackage.haskell.org/package/servant-elm): Generate Elm functions to query your Servant API
    
and more, refer [package.yaml](https://github.com/matsubara0507/anaqram-server/blob/master/package.yaml).
 
## Usage
 
Installed haskell stack, elm compiler, and postgresql.
 
```
$ stack ghci
ghci> DB.doMigration -- migaration for postgresql
ghci> :q
$ stack test
$ stack exec -- server
```

access `localhost:8080`.

when exec `stack test`, generate Elm soarce code and compile Elm. 
