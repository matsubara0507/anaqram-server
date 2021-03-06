FROM codesimple/elm:0.19 as build-elm
WORKDIR /work
COPY . /work
RUN elm make elm-src/Main.elm --output=static/main.js
RUN elm make elm-src/ScoreBoard.elm --output=static/scoreboard/main.js

FROM matsubara0507/ubuntu-for-haskell
ARG local_bin_path
WORKDIR /work
COPY ${local_bin_path} /usr/local/bin
COPY --from=build-elm /work/static /work/static
