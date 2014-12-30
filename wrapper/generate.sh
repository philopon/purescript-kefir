#!/bin/bash

cd `dirname $0`

OUT=../src/FRP/Kefir/Foreign.purs

../node_modules/webpack/bin/webpack.js --output-library kefir kefir bundle.js 1>&2

cat <<EOC > $OUT
module FRP.Kefir.Foreign (kefir) where

foreign import kefir """
EOC

cat bundle.js | tr -d '\r' >> $OUT

cat <<EOC >> $OUT
""" :: forall a. a
EOC
