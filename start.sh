#!/bin/bash

set -exuo pipefail

[ -e $BG_FILE_PATH ] || cp default-bg.png $BG_FILE_PATH
./build/dndgrid
