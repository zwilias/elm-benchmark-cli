#!/bin/bash

elm-make --yes Main.elm --output elm.js >/dev/null && node run.js
