#!/bin/bash

OUT=src/FRP/Kefir/Foreign.purs

cat <<EOC > $OUT
module FRP.Kefir.Foreign (kefir) where

foreign import kefir """
var kefir = (function(module, exports){
EOC

cat node_modules/kefir/dist/kefir.js >> $OUT

cat <<EOC >> $OUT

return module.exports;
}({}, {}));""" :: forall a. a
EOC
