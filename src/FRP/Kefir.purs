module FRP.Kefir
  ( Kefir(), Stream(), Property(), EffKefir()

  , StreamLike
  , Terminable, onEnd
  , Observable, onValue
  , IsStream
  , IsProperty

  , FunKey(), off
  , onLog, offLog

  , Emitter(), emitter, emit, end
  , Never(), never
  , Later(), later
  , Interval(), interval
  , Sequentially(), sequentially
  , Repeatedly(), repeatedly
  , FromPoll(), fromPoll
  , WithInterval(), withInterval
  , FromCallback(), fromCallback
  , FromBinder(), fromBinder

  , Constant(), constant

  , toProperty, toPropertyWith
  , changes
  , withDefault

  , map, mapEff
  , filter, filterEff
  , take, takeWhile, takeWhileEff
  , skip, skipWhile, skipWhileEff
  , skipDuplicatesWith, skipDuplicates
  , diff1, diff
  , scan1, scan
  , reduce1, reduce, reduceEff1, reduceEff
  , mapEnd, SkipEnd(), skipEnd
  , Min(), Max(), slidingWindow
  ) where

import Control.Monad.Eff
import FRP.Kefir.Foreign
import Data.Function

foreign import data Kefir   :: !
foreign import data Stream   :: * -> *
foreign import data Property :: * -> *
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
  }""" :: forall e a. Eff e a -> a

foreign import wrap """
function wrap(m){
  return function wrapEff(){
    return m();
  }
}""" :: forall e a. Eff e a -> Eff e a

-- Stream
class  StreamLike (stream :: * -> *)
class (StreamLike stream) <= Terminable stream
class (StreamLike stream) <= Observable stream

class (StreamLike stream) <= IsStream   stream
class (StreamLike stream) <= IsProperty stream

instance streamLikeStream :: StreamLike Stream
instance terminableStream :: Terminable Stream
instance observableStream :: Observable Stream
instance isStreamStream   :: IsStream   Stream

instance streamLikeProperty :: StreamLike Property
instance terminableProperty :: Terminable Property
instance observableProperty :: Observable Property
instance isPropertyProperty :: IsProperty Property

-- Emitter
newtype Emitter a = Emitter (Stream a)
emitter :: forall e a. EffKefir e (Emitter a)
emitter = runFn2 call0Eff "emitter" kefir

emit :: forall e a. Emitter a -> a -> EffKefir e Unit
emit = runFn3 call1Eff "emit"

end :: forall e a. Emitter a -> EffKefir e Unit
end = runFn2 call0Eff "end"

instance streamLikeEmitter :: StreamLike Emitter
instance terminableEmitter :: Terminable Emitter
instance observableEmitter :: Observable Emitter
instance isStreamEmitter   :: IsStream   Emitter

-- never
newtype Never a = Never (Stream a)
never :: forall e a. EffKefir e (Never a)
never = runFn2 call0Eff "never" kefir

instance streamLikeNever :: StreamLike Never
instance terminableNever :: Terminable Never
instance isStreamNever   :: IsStream   Never

-- later
newtype Later a = Later (Stream a)
later :: forall e a. Number -> a -> EffKefir e (Later a)
later = runFn4 call2Eff "later" kefir

instance streamLikeLater :: StreamLike Later
instance terminableLater :: Terminable Later
instance observableLater :: Observable Later
instance isStreamLater   :: IsStream   Later

-- Interval
newtype Interval a = Interval (Stream a)
interval :: forall e a. Number -> a -> EffKefir e (Interval a)
interval = runFn4 call2Eff "interval" kefir

instance streamLikeInterval :: StreamLike Interval
instance observableInterval :: Observable Interval
instance isStreamInterval   :: IsStream   Interval

-- sequentially
newtype Sequentially a = Sequentially (Stream a)
sequentially :: forall e a. Number -> [a] -> EffKefir e (Sequentially a)
sequentially = runFn4 call2Eff "sequentially" kefir

instance streamLikeSequentially :: StreamLike Sequentially
instance terminableSequentially :: Terminable Sequentially
instance observableSequentially :: Observable Sequentially
instance isStreamSequentially   :: IsStream   Sequentially

-- repeatedly
newtype Repeatedly a = Repeatedly (Stream a)
repeatedly :: forall e a. Number -> [a] -> EffKefir e (Repeatedly a)
repeatedly = runFn4 call2Eff "repeatedly" kefir

instance streamLikeRepeatedly :: StreamLike Repeatedly
instance observableRepeatedly :: Observable Repeatedly
instance isStreamRepeatedly   :: IsStream   Repeatedly

-- fromPoll
newtype FromPoll a = FromPoll (Stream a)
fromPoll :: forall e a. Number -> EffKefir e a -> EffKefir e (FromPoll a)
fromPoll = runFn4 call2Eff "fromPoll" kefir

instance streamLikeFromPoll :: StreamLike FromPoll
instance observableFromPoll :: Observable FromPoll
instance isStreamFromPoll   :: IsStream   FromPoll

-- withinterval
newtype WithInterval a = WithInterval (Stream a)
withInterval :: forall e a. Number
             -> (Emitter a -> EffKefir e Unit)
             -> EffKefir e (WithInterval a)
withInterval i f = runFn4 call2Eff "withInterval" kefir i (\e -> execute $ f e)

instance streamLikeWithInterval :: StreamLike WithInterval
instance observableWithInterval :: Observable WithInterval
instance terminableWithInterval :: Terminable WithInterval
instance isStreamWithInterval   :: IsStream   WithInterval

-- fromCallback
newtype FromCallback a = FromCallback (Stream a)
fromCallback :: forall e a. ((a -> EffKefir e Unit) -> EffKefir e Unit) -> EffKefir e (FromCallback a)
fromCallback m = runFn3 call1Eff "fromCallback" kefir (\e -> execute $ m (\a -> return $ e a))

instance streamLikeFromCallback :: StreamLike FromCallback
instance observableFromCallback :: Observable FromCallback
instance terminableFromCallback :: Terminable FromCallback
instance isStreamFromCallback   :: IsStream   FromCallback

-- fromBinder
newtype FromBinder a = FromBinder (Stream a)
fromBinder :: forall e a. (Emitter a -> EffKefir e (EffKefir e Unit))
           -> EffKefir e (FromBinder a)
fromBinder f = runFn3 call1Eff "fromBinder" kefir (\e -> execute $ f e)

instance streamLikeFromBinder :: StreamLike FromBinder
instance observableFromBinder :: Observable FromBinder
instance terminableFromBinder :: Terminable FromBinder
instance isStreamFromBinder   :: IsStream   FromBinder

-- Property
newtype Constant a = Constant (Property a)
constant :: forall e a. a -> EffKefir e (Constant a)
constant = runFn3 call1Eff "constant" kefir

instance streamLikeConstant :: StreamLike Constant
instance observableConstant :: Observable Constant
instance terminableConstant :: Terminable Constant
instance isPropertyConstant :: IsProperty Constant

-- Observable Impl

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

onValue :: forall e stream a. (Observable stream) => stream a -> (a -> EffKefir e _) -> EffKefir e FunKey
onValue = runFn3 onValueImpl targetValue

onEnd :: forall e stream a. (Terminable stream) => stream a -> (EffKefir e _) -> EffKefir e FunKey
onEnd = runFn3 onEndImpl targetEnd

onLog :: forall e stream. (StreamLike stream) => stream _ -> String -> EffKefir e Unit
onLog = runFn3 call1Eff "log"

offLog :: forall e stream. (StreamLike stream) => stream _ -> EffKefir e Unit
offLog = runFn2 call0Eff "offLog"

toProperty :: forall e stream a. (IsStream stream) => stream a -> EffKefir e (Property a)
toProperty = runFn2 call0Eff "toProperty"

toPropertyWith :: forall e stream a. (IsStream stream) => a -> stream a -> EffKefir e (Property a)
toPropertyWith w s = runFn3 call1Eff "toProperty" s w

changes :: forall e stream a. (IsProperty stream) => stream a -> EffKefir e (Stream a)
changes = runFn2 call0Eff "changes"

withDefault :: forall e stream a. (StreamLike stream) => a -> stream a -> EffKefir e (Property a)
withDefault d s = runFn3 call1Eff "withDefault" s d

-- modify an observable

map :: forall e stream a b. (StreamLike stream) => (a -> b) -> stream a -> EffKefir e (Stream b)
map f s = runFn3 call1Eff "map" s f

mapEff :: forall e stream a b. (StreamLike stream) => (a -> EffKefir e b) -> stream a -> EffKefir e (Stream b)
mapEff f s = runFn3 call1Eff "map" s (\v -> execute (f v))

filter :: forall e stream a. (StreamLike stream) => (a -> Boolean) -> stream a -> EffKefir e (Stream a)
filter f s = runFn3 call1Eff "filter" s f

filterEff :: forall e stream a. (StreamLike stream) => (a -> EffKefir e Boolean) -> stream a -> EffKefir e (Stream a)
filterEff f s = runFn3 call1Eff "filter" s (\v -> execute (f v))

take :: forall e stream a. (StreamLike stream) => Number -> stream a -> EffKefir e (Stream a)
take n s = runFn3 call1Eff "take" s n

takeWhile :: forall e stream a. (StreamLike stream) => (a -> Boolean) -> stream a -> EffKefir e (Stream a)
takeWhile f s = runFn3 call1Eff "takeWhile" s f

takeWhileEff :: forall e stream a. (StreamLike stream) => (a -> EffKefir e Boolean) -> stream a -> EffKefir e (Stream a)
takeWhileEff f s = runFn3 call1Eff "takeWhile" s (\v -> execute (f v))

skip :: forall e stream a. (StreamLike stream) => Number -> stream a -> EffKefir e (Stream a)
skip n s = runFn3 call1Eff "skip" s n

skipWhile :: forall e stream a. (StreamLike stream) => (a -> Boolean) -> stream a -> EffKefir e (Stream a)
skipWhile f s = runFn3 call1Eff "skipWhile" s f

skipWhileEff :: forall e stream a. (StreamLike stream) => (a -> EffKefir e Boolean) -> stream a -> EffKefir e (Stream a)
skipWhileEff f s = runFn3 call1Eff "skipWhile" s (\v -> execute (f v))

skipDuplicatesWith :: forall e stream a. (StreamLike stream) => (a -> a -> Boolean) -> stream a -> EffKefir e (Stream a)
skipDuplicatesWith f s = runFn3 call1Eff "skipDuplicates" s (mkFn2 f)

skipDuplicates :: forall e stream a. (StreamLike stream, Eq a) => stream a -> EffKefir e (Stream a)
skipDuplicates = skipDuplicatesWith (==)

diff1 :: forall e stream a b. (StreamLike stream) => (a -> a -> b) -> stream a -> EffKefir e (Stream b)
diff1 f s = runFn3 call1Eff "diff" s (mkFn2 f)

diff :: forall e stream a b. (StreamLike stream) => (a -> a -> b) -> a -> stream a -> EffKefir e (Stream b)
diff f a s = runFn4 call2Eff "diff" s (mkFn2 f) a

scan1 :: forall e stream a. (StreamLike stream) => (a -> a -> a) -> stream a -> EffKefir e (Stream a)
scan1 f s = runFn3 call1Eff "scan" s (mkFn2 f)

scan :: forall e stream a b. (StreamLike stream) => (b -> a -> b) -> b -> stream a -> EffKefir e (Stream b)
scan f a s = runFn4 call2Eff "scan" s (mkFn2 f) a

reduce1 :: forall e stream a. (StreamLike stream) => (a -> a -> a) -> stream a -> EffKefir e (Stream a)
reduce1 f s = runFn3 call1Eff "reduce" s (mkFn2 f)

reduce :: forall e stream a b. (StreamLike stream) => (b -> a -> b) -> b -> stream a -> EffKefir e (Stream b)
reduce f a s = runFn4 call2Eff "reduce" s (mkFn2 f) a

reduceEff1 :: forall e stream a. (StreamLike stream) => (a -> a -> EffKefir e a) -> stream a -> EffKefir e (Stream a)
reduceEff1 f s = runFn3 call1Eff "reduce" s (mkFn2 (\a b -> execute (f a b)))

reduceEff :: forall e stream a b. (StreamLike stream) => (b -> a -> EffKefir e b) -> b -> stream a -> EffKefir e (Stream b)
reduceEff f a s = runFn4 call2Eff "reduce" s (mkFn2 (\a b -> execute (f a b))) a

mapEnd :: forall e stream a. EffKefir e a -> stream a -> EffKefir e (Stream a)
mapEnd f s = runFn3 call1Eff "mapEnd" s (wrap f)

newtype SkipEnd a = SkipEnd (Stream a)
instance streamLikeSkipEnd :: StreamLike SkipEnd
instance observableSkipEnd :: Observable SkipEnd
instance isStreamSkipEnd   :: IsStream   SkipEnd

skipEnd :: forall e stream a. stream a -> EffKefir e (SkipEnd a)
skipEnd = runFn2 call0Eff "skipEnd"

type Min = Number
type Max = Number
slidingWindow :: forall e stream a. Min -> Max -> stream a -> EffKefir e (Stream [a])
slidingWindow min max s = runFn4 call2Eff "slidingWindow" s max min
