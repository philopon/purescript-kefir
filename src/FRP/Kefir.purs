module FRP.Kefir where

import Control.Monad.Eff
import FRP.Kefir.Foreign
import Data.Function

foreign import data Kefir   :: !
foreign import data Stream  :: * -> *
foreign import data RegisteredFunction :: *

type EffKefir e = Eff (kefir :: Kefir | e)

foreign import call0Eff """
  function call0Eff(f, obj) {
    return function(){
      return obj[f]();
    }
  }""" :: forall e o r. Fn2 String o (Eff e r)

foreign import call1Eff """
  function call1Eff(f, obj, a) {
    return function(){
      return obj[f](a);
    }
  }""" :: forall e o a r. Fn3 String o a (Eff e r)

foreign import call2Eff """
  function call2Eff(f, obj, a, b) {
    return function(){
      return obj[f](a, b);
    }
  }""" :: forall e o a b r. Fn4 String o a b (Eff e r)

foreign import execute """
  function execute(m){
    return m();
  }""" :: forall a. a -> a

newtype Emitter a = Emitter (Stream a)
newtype Endless a = Endless (Stream a)

emitter :: forall e a. EffKefir e (Emitter a)
emitter = runFn2 call0Eff "emitter" kefir

never :: forall e a. EffKefir e (Stream a)
never = runFn2 call0Eff "never" kefir

later :: forall e a. Number -> a -> EffKefir e (Stream a)
later = runFn4 call2Eff "later" kefir

interval :: forall e a. Number -> a -> EffKefir e (Endless a)
interval = runFn4 call2Eff "interval" kefir

sequentially :: forall e a. Number -> [a] -> EffKefir e (Stream a)
sequentially = runFn4 call2Eff "sequentially" kefir

repeatedly :: forall e a. Number -> [a] -> EffKefir e (Endless a)
repeatedly = runFn4 call2Eff "repeatedly" kefir

fromPoll :: forall e a. Number -> EffKefir e a -> EffKefir e (Endless a)
fromPoll = runFn4 call2Eff "fromPoll" kefir

withInterval :: forall e a. Number
             -> (Emitter a -> EffKefir e Unit)
             -> EffKefir e (Stream a)
withInterval i f = runFn4 call2Eff "withInterval" kefir i (\e -> execute $ f e)

fromCallback :: forall e a. ((a -> EffKefir e Unit) -> EffKefir e Unit) -> EffKefir e (Stream a)
fromCallback m = runFn3 call1Eff "fromCallback" kefir (\e -> execute $ m (\a -> return $ e a))

class Emittable stream where
  emit :: forall e a. stream a -> a -> EffKefir e Unit

instance emittableStream :: Emittable Emitter where
  emit = runFn3 call1Eff "emit"

class Endable stream where
  end :: forall e a. stream a -> EffKefir e Unit

instance endableStream :: Endable Emitter where
  end = runFn2 call0Eff "end"

newtype Target = Target Number

targetValue :: Target
targetValue = Target 0

targetEnd :: Target
targetEnd = Target 1

type Targets = {value :: Target, end :: Target}

targets :: Targets
targets = {value: targetValue, end: targetEnd }

foreign import data DummyStream :: *
newtype FunKey = FunKey {stream :: DummyStream, target :: Target, function :: RegisteredFunction}

foreign import onValueImpl """
function onValueImpl(tgt, str, fn){
  return function onValueImplEff(){
    function onValueCallback(x){
      fn(x)();
    };
    str.onValue(onValueCallback);
    return {stream: str, target: tgt, function: onValueCallback}
  }
}""" :: forall stream e a b. Fn3 Target (stream a) (a -> EffKefir e b) (EffKefir e FunKey)

foreign import onEndImpl """
function onEndImpl(tgt, str, fn){
  return function onEndImplEff(){
    function onEndCallback(){
      fn();
    }
    str.onEnd(onEndCallback);
    return {stream: str, target: tgt, function: onEndCallback}
  }
}""" :: forall stream e a b. Fn3 Target (stream a) (EffKefir e b) (EffKefir e FunKey)

foreign import offImpl """
function offImpl(targets, key){
  return function offImplEff() {
    if(key.target === targets.value) {
      key.stream.offValue(key.function);
    } else {
      key.stream.offEnd(key.function);
    }
  }
}""" :: forall e. Fn2 Targets FunKey (EffKefir e Unit)

off :: forall e. FunKey -> EffKefir e Unit
off = runFn2 offImpl targets

class Limitted stream where
  onEnd   :: forall e a b. stream a -> (EffKefir e b) -> EffKefir e FunKey

instance limittedEmitter :: Limitted Emitter where
  onEnd = runFn3 onEndImpl targetEnd

instance limittedStream :: Limitted Stream where
  onEnd = runFn3 onEndImpl targetEnd

class Observable stream where
  onValue :: forall e a b. stream a -> (a -> EffKefir e b) -> EffKefir e FunKey
  onLog   :: forall e a. stream a -> String -> EffKefir e Unit
  offLog  :: forall e a. stream a -> EffKefir e Unit

instance observableEmitter :: Observable Emitter where
  onValue = runFn3 onValueImpl targetValue
  onLog   = runFn3 call1Eff "log"
  offLog  = runFn2 call0Eff "offLog"

instance observableEndless :: Observable Endless where
  onValue = runFn3 onValueImpl targetValue
  onLog   = runFn3 call1Eff "log"
  offLog  = runFn2 call0Eff "offLog"

instance observableStream :: Observable Stream where
  onValue = runFn3 onValueImpl targetValue
  onLog   = runFn3 call1Eff "log"
  offLog  = runFn2 call0Eff "offLog"
