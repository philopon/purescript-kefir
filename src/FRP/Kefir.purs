module FRP.Kefir
  ( Kefir(), Stream(), Property(), EffKefir()

  , StreamLike
  , Unregister()
  , Terminable, onEnd
  , Observable, onValue
  , Emittable, emit, end
  , Pluggable, plug, unPlug
  , IsStream
  , IsProperty
  , forget

  , Event(..), onAny
  , onLog, offLog

  , Emitter(), emitter
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
  , bufferWhile, bufferWhileWith
  , delay
  , throttle, throttleWith
  , debounce, debounceWith
  , flatten, flattenWith
  , withHandler

  , combine
  , and, or
  , sampledBy
  , zipWith
  , merge
  , concat
  , Pool(), pool
  , Bus(), bus
  , flatMap, flatMapLatest, flatMapFirst, flatMapConcat
  , flatMapWith, flatMapLatestWith, flatMapFirstWith, flatMapConcatWith
  , flatMapConcurLimit, flatMapConcurLimitWith

  , filterBy
  , takeWhileBy
  , skipWhileBy
  , skipUntilBy
  , takeUntilBy
  , bufferBy
  , bufferWhileBy
  , awaiting
  ) where

import Control.Monad.Eff
import FRP.Kefir.Foreign
import Data.Function

foreign import data Kefir   :: !
foreign import data Stream   :: * -> *
foreign import data Property :: * -> *

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

foreign import call3Eff """
  function call3Eff(f, obj, a, b, c) {
    return function(){
      return obj[f](a, b, c);
    }
  }""" :: forall e o a b c r. Fn5 String o a b c (Eff e r)

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

class (StreamLike stream) <= Emittable stream
class (StreamLike stream) <= Pluggable stream

instance streamLikeStream :: StreamLike Stream
instance terminableStream :: Terminable Stream
instance observableStream :: Observable Stream
instance isStreamStream   :: IsStream   Stream

instance streamLikeProperty :: StreamLike Property
instance terminableProperty :: Terminable Property
instance observableProperty :: Observable Property
instance isPropertyProperty :: IsProperty Property

foreign import forgetImpl """
function forgetImpl(stream){
  return stream;
}""" :: forall stream a b. stream a -> Stream b

forget :: forall stream a. (StreamLike stream) => stream a -> Stream a
forget = forgetImpl

unsafeForget :: forall stream a b. (StreamLike stream) => stream a -> Stream b
unsafeForget = forgetImpl

-- Emitter
newtype Emitter a = Emitter (Stream a)
emitter :: forall e a. EffKefir e (Emitter a)
emitter = runFn2 call0Eff "emitter" kefir

emit :: forall e stream a. (Emittable stream) => stream a -> a -> EffKefir e Unit
emit = runFn3 call1Eff "emit"

end :: forall e stream. (Emittable stream) => stream _ -> EffKefir e Unit
end = runFn2 call0Eff "end"

instance streamLikeEmitter :: StreamLike Emitter
instance terminableEmitter :: Terminable Emitter
instance observableEmitter :: Observable Emitter
instance emittableEmitter  :: Emittable  Emitter
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
type Unregister e = EffKefir e Unit

foreign import onValueImpl """
function onValueImpl(str, fn){
  return function onValueImplEff(){
    function onValueCallback(x){
      fn(x)();
    };
    str.onValue(onValueCallback);
    return function offValueEff() {
      str.offValue(onValueCallback);
    }
  }
}""" :: forall stream e a b. Fn2 (stream a) (a -> EffKefir e b) (EffKefir e (Unregister e))

onValue :: forall e stream a. (Observable stream) => stream a -> (a -> EffKefir e _) -> EffKefir e (Unregister e)
onValue = runFn2 onValueImpl

foreign import onEndImpl """
function onEndImpl(str, fn){
  return function onEndImplEff(){
    function onEndCallback(){
      fn();
    }
    str.onEnd(onEndCallback);
    return function offEndEff() {
      str.offEnd(onEndCallback);
    }
  }
}""" :: forall stream e a b. Fn2 (stream a) (EffKefir e b) (EffKefir e (Unregister e))

onEnd :: forall e stream a. (Terminable stream) => stream a -> (EffKefir e _) -> EffKefir e (Unregister e)
onEnd = runFn2 onEndImpl

data Event a
  = Value Boolean a
  | End

foreign import onAnyImpl """
function onAnyImpl(cnsts, str, fn){
  return function onAnyImplEff(){
    function onAnyCallback(ev){
      var v = ev.type === 'value'
        ? cnsts.value(ev.current, ev.value)
        : cnsts.end;
      return fn(v)();
    }
    str.onAny(onAnyCallback);
    return function offAnyEff(){
      str.offAny(onAnyCallback);
    }
  }
}""" :: forall e stream a b. Fn3
  {value :: Fn2 Boolean a (Event a), end :: Event a}
  (stream a) (Event a -> EffKefir e b) (EffKefir e (Unregister e))

onAny :: forall e stream a. (Terminable stream, Observable stream) => stream a -> (Event a -> EffKefir e _) -> EffKefir e (Unregister e)
onAny = runFn3 onAnyImpl {value: mkFn2 Value, end: End}

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

bufferWhileWith :: forall e stream a. {flushOnEnd :: Boolean} -> (a -> Boolean) -> stream a -> EffKefir e (Stream [a])
bufferWhileWith opts f s = runFn4 call2Eff "bufferWhile" s f opts

bufferWhile :: forall e stream a. (a -> Boolean) -> stream a -> EffKefir e (Stream [a])
bufferWhile f s = runFn3 call1Eff "bufferWhile" s f

delay :: forall e stream a. Number -> stream a -> EffKefir e (Stream a)
delay w s = runFn3 call1Eff "delay" s w

throttleWith :: forall e stream a. {leading :: Boolean, trailing :: Boolean} -> Number -> stream a -> EffKefir e (Stream a)
throttleWith opts w s = runFn4 call2Eff "throttle" s w opts

throttle :: forall e stream a. Number -> stream a -> EffKefir e (Stream a)
throttle w s = runFn3 call1Eff "throttle" s w

debounceWith :: forall e stream a. {immediate :: Boolean} -> Number -> stream a -> EffKefir e (Stream a)
debounceWith opts w s = runFn4 call2Eff "debounce" s w opts

debounce :: forall e stream a. Number -> stream a -> EffKefir e (Stream a)
debounce w s = runFn3 call1Eff "debounce" s w

flatten :: forall e stream a. stream [a] -> EffKefir e (Stream a)
flatten = runFn2 call0Eff "flatten"

flattenWith :: forall e stream a b. (a -> [b]) -> stream a -> EffKefir e (Stream b)
flattenWith f s = runFn3 call1Eff "flatten" s f

foreign import withHandlerImpl """
function withHandlerImpl(cnsts, src, fun){
  return function withHandlerImplEff(){
    return src.withHandler(function(emitter, ev){
      var v = ev.type === 'value'
        ? cnsts.value(ev.current, ev.value)
        : cnsts.end;
      fun(emitter, v)();
    });
  }
}""" :: forall e stream a b c. Fn3
  {value :: Fn2 Boolean a (Event a), end :: Event a}
  (stream a)
  (Fn2 (Emitter b) (Event a) (EffKefir e c))
  (EffKefir e (Stream b))

withHandler :: forall e stream a b. (StreamLike stream) => stream a -> (Emitter b -> Event a -> EffKefir e _) -> EffKefir e (Stream b)
withHandler s f = runFn3 withHandlerImpl {value: mkFn2 Value, end: End} s (mkFn2 f)

-- Combine

-- TODO: combine multi stream
combine :: forall e streamA streamB a b x. (StreamLike streamA, StreamLike streamB) => streamA a -> streamB b -> (a -> b -> x) -> EffKefir e (Stream x)
combine a b f = runFn4 call2Eff "combine" kefir [unsafeForget a, unsafeForget b] (mkFn2 f)

and :: forall stream e. (StreamLike stream) => [stream Boolean] -> EffKefir e (Stream Boolean)
and = runFn3 call1Eff "and" kefir

or :: forall stream e. (StreamLike stream) => [stream Boolean] -> EffKefir e (Stream Boolean)
or = runFn3 call1Eff "or" kefir

-- TODO: sampledBy multi stream
sampledBy :: forall e passive active a b x. (StreamLike passive, StreamLike active) => passive a -> active b -> (a -> b -> x) -> EffKefir e (Stream x)
sampledBy pass act f = runFn5 call3Eff "sampledBy" kefir [pass] [act] (mkFn2 f)

-- TODO: zip multi stream
zipWith :: forall e streamA streamB a b x. (StreamLike streamA, StreamLike streamB) => (a -> b -> x) -> streamA a -> streamB b -> EffKefir e (Stream x)
zipWith f a b = runFn4 call2Eff "zip" kefir [unsafeForget a, unsafeForget b] (mkFn2 f)

merge :: forall stream e a. (StreamLike stream) => [stream a] -> EffKefir e (Stream a)
merge = runFn3 call1Eff "merge" kefir

concat :: forall stream e a. (StreamLike stream) => [stream a] -> EffKefir e (Stream a)
concat = runFn3 call1Eff "concat" kefir

newtype Pool a = Pool (Stream a)
instance streamLikePool :: StreamLike Pool
instance observablePool :: Observable Pool
instance pluggablePool  :: Pluggable  Pool
instance isStreamPool   :: IsStream   Pool

pool :: forall e a. EffKefir e (Pool a)
pool = runFn2 call0Eff "pool" kefir

plug :: forall e hub stream a. (Pluggable hub, StreamLike stream) => hub a -> stream a -> EffKefir e Unit
plug = runFn3 call1Eff "plug"

unPlug :: forall e hub stream a. (Pluggable hub, StreamLike stream) => hub a -> stream a -> EffKefir e Unit
unPlug = runFn3 call1Eff "unplug"

newtype Bus a = Bus (Stream a)
instance streamLikeBus :: StreamLike Bus
instance observableBus :: Observable Bus
instance terminableBus :: Terminable Bus
instance pluggableBus  :: Pluggable  Bus
instance emittableBus  :: Emittable  Bus
instance isStreamBus   :: IsStream   Bus

bus :: forall e a. EffKefir e (Bus a)
bus = runFn2 call0Eff "bus" kefir

flatMap :: forall e stream child a. (StreamLike stream, StreamLike child) => stream (child a) -> EffKefir e (Stream a)
flatMap = runFn2 call0Eff "flatMap"

flatMapLatest :: forall e stream child a. (StreamLike stream, StreamLike child) => stream (child a) -> EffKefir e (Stream a)
flatMapLatest = runFn2 call0Eff "flatMapLatest"

flatMapFirst :: forall e stream child a. (StreamLike stream, StreamLike child) => stream (child a) -> EffKefir e (Stream a)
flatMapFirst = runFn2 call0Eff "flatMapFirst"

flatMapConcat :: forall e stream child a. (StreamLike stream, StreamLike child) => stream (child a) -> EffKefir e (Stream a)
flatMapConcat = runFn2 call0Eff "flatMapConcat"

flatMapWith :: forall e stream child a b. (StreamLike stream, StreamLike child) => stream a -> (a -> EffKefir e (child b)) -> EffKefir e (Stream b)
flatMapWith s f = runFn3 call1Eff "flatMap" s (execute <<< f)

flatMapLatestWith :: forall e stream child a b. (StreamLike stream, StreamLike child) => stream a -> (a -> EffKefir e (child b)) -> EffKefir e (Stream b)
flatMapLatestWith s f = runFn3 call1Eff "flatMapLatest" s (execute <<< f)

flatMapFirstWith :: forall e stream child a b. (StreamLike stream, StreamLike child) => stream a -> (a -> EffKefir e (child b)) -> EffKefir e (Stream b)
flatMapFirstWith s f = runFn3 call1Eff "flatMapFirst" s (execute <<< f)

flatMapConcatWith :: forall e stream child a b. (StreamLike stream, StreamLike child) => stream a -> (a -> EffKefir e (child b)) -> EffKefir e (Stream b)
flatMapConcatWith s f = runFn3 call1Eff "flatMapConcat" s (execute <<< f)

foreign import undefined "var undefined = undefined;" :: forall a. a

flatMapConcurLimit :: forall e stream child a. (StreamLike stream, StreamLike child) => Number -> stream (child a) -> EffKefir e (Stream a)
flatMapConcurLimit l s = runFn4 call2Eff "flatMapConcurLimit" s undefined l

flatMapConcurLimitWith :: forall e stream child a b. (StreamLike stream, StreamLike child) => Number -> stream a -> (a -> EffKefir e (child b)) -> EffKefir e (Stream b)
flatMapConcurLimitWith l s f = runFn4 call2Eff "flatMapConcurLimit" s (execute <<< f) l

-- combine two observables
filterBy :: forall e stream filter a. (StreamLike stream, StreamLike filter) => stream a -> filter Boolean -> EffKefir e (Stream a)
filterBy v f = runFn3 call1Eff "filterBy" v f

takeWhileBy :: forall e stream filter a. (StreamLike stream, StreamLike filter) => stream a -> filter Boolean -> EffKefir e (Stream a)
takeWhileBy v f = runFn3 call1Eff "takeWhileBy" v f

skipWhileBy :: forall e stream filter a. (StreamLike stream, StreamLike filter) => stream a -> filter Boolean -> EffKefir e (Stream a)
skipWhileBy v f = runFn3 call1Eff "skipWhileBy" v f

skipUntilBy :: forall e stream filter a. (StreamLike stream, StreamLike filter) => stream a -> filter _ -> EffKefir e (Stream a)
skipUntilBy v f = runFn3 call1Eff "skipUntilBy" v f

takeUntilBy :: forall e stream filter a. (StreamLike stream, StreamLike filter) => stream a -> filter _ -> EffKefir e (Stream a)
takeUntilBy v f = runFn3 call1Eff "takeUntilBy" v f

bufferBy :: forall e stream filter a. (StreamLike stream, StreamLike filter) => stream a -> filter _ -> EffKefir e (Stream [a])
bufferBy v f = runFn3 call1Eff "bufferBy" v f

bufferWhileBy :: forall e stream filter a. (StreamLike stream, StreamLike filter) => stream a -> filter Boolean -> EffKefir e (Stream [a])
bufferWhileBy v f = runFn3 call1Eff "bufferWhileBy" v f

awaiting :: forall e on off. (StreamLike on, StreamLike off) => on _ -> off _ -> EffKefir e (Stream Boolean)
awaiting v f = runFn3 call1Eff "awaiting" v f
