module FRP.Kefir
  ( Kefir(), Stream(), Property(), EffKefir()
  , Terminable(), Observable(), Emittable(), Pluggable()
  , HasE(), HasO(), HasP(), HasT()
  , E(), O(), P(), T(), OT(), EP()

  , StreamLike
  , Unregister()
  , onEnd
  , onValue
  , emit, end
  , emitAsync, endAsync
  , plug, unPlug
  , forget

  , Event(..), onAny
  , onLog, offLog

  , emitter
  , never
  , later
  , interval
  , sequentially
  , repeatedly
  , fromPoll
  , withInterval
  , fromCallback
  , fromBinder

  , constant

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
  , mapEnd, skipEnd
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
  , pool
  , bus
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

  , unsafeGlobalize
  ) where

import Control.Monad.Eff
import FRP.Kefir.Foreign
import Data.Function

foreign import data Kefir    :: !
foreign import data Stream   :: #* -> #* -> * -> *
foreign import data Property :: #* -> #* -> * -> *

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

foreign import call0EffAsync """
  function call0EffAsync(f, obj) {
    return function(){
      return setTimeout(function(){obj[f]()}, 0);
    }
  }""" :: forall e o. Fn2 String o (Eff e Unit)

foreign import call1EffAsync """
  function call1EffAsync(f, obj, a) {
    return function(){
      return setTimeout(function(){obj[f](a)}, 0);
    }
  }""" :: forall e o a. Fn3 String o a (Eff e Unit)

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
foreign import data Terminable :: *
foreign import data Observable :: *
foreign import data Emittable  :: *
foreign import data Pluggable  :: *

type HasO s = (observable :: Observable | s)
type HasT s = (terminable :: Terminable | s)
type O    = HasO ()
type T    = HasT ()
type OT   = (observable :: Observable, terminable :: Terminable)

type HasE s = (emittable  :: Emittable | s)
type HasP s = (pluggable  :: Pluggable | s)

type E    = HasE ()
type P    = HasP ()
type EP   = (emittable :: Emittable, pluggable :: Pluggable)

class StreamLike (stream :: #* -> #* -> * -> *)
instance streamLikeStream   :: StreamLike Stream
instance streamLikeProperty :: StreamLike Property

foreign import forgetImpl """
function forgetImpl(stream){
  return stream;
}""" :: forall str str' s s' a b. str s a -> str' s' b

asStream :: forall stream s a. (StreamLike stream) => stream _ s a -> Stream _ s a
asStream = forgetImpl

forget :: forall stream a. (StreamLike stream) => stream _ _ a -> Stream _ _ a
forget = forgetImpl

unsafeForget :: forall stream. (StreamLike stream) => stream _ _ _ -> Stream _ _ _
unsafeForget = forgetImpl

unsafeGlobalize :: forall stream p s a. (StreamLike stream) => EffKefir _ (stream p s a) -> stream p s a
unsafeGlobalize = execute

-- Emitter
emitter :: EffKefir _ (Stream E OT _)
emitter = runFn2 call0Eff "emitter" kefir

emit :: forall a. Stream (HasE _) _ a -> a -> EffKefir _ Unit
emit = runFn3 call1Eff "emit"

end :: Stream (HasE _) _ _ -> EffKefir _ Unit
end = runFn2 call0Eff "end"

emitAsync :: forall a. Stream (HasE _) _ a -> a -> EffKefir _ Unit
emitAsync = runFn3 call1EffAsync "emit"

endAsync :: Stream (HasE _) _ _ -> EffKefir _ Unit
endAsync = runFn2 call0EffAsync "end"

-- never
never :: EffKefir _ (Stream () T _)
never = runFn2 call0Eff "never" kefir

-- later
later :: forall a. Number -> a -> EffKefir _ (Stream () OT a)
later = runFn4 call2Eff "later" kefir

-- Interval
interval :: forall a. Number -> a -> EffKefir _ (Stream () O a)
interval = runFn4 call2Eff "interval" kefir

-- sequentially
sequentially :: forall a. Number -> [a] -> EffKefir _ (Stream () OT a)
sequentially = runFn4 call2Eff "sequentially" kefir

-- repeatedly
repeatedly :: forall a. Number -> [a] -> EffKefir _ (Stream () O a)
repeatedly = runFn4 call2Eff "repeatedly" kefir

-- fromPoll
fromPoll :: forall e a. Number -> EffKefir e a -> EffKefir e (Stream () O a)
fromPoll = runFn4 call2Eff "fromPoll" kefir

-- withinterval
withInterval :: forall e a. Number -> (Stream E () a -> EffKefir e Unit) -> EffKefir e (Stream () OT a)
withInterval i f = runFn4 call2Eff "withInterval" kefir i (\e -> execute $ f e)

-- fromCallback
fromCallback :: forall e a. ((a -> EffKefir e Unit) -> EffKefir e Unit) -> EffKefir e (Stream () OT a)
fromCallback m = runFn3 call1Eff "fromCallback" kefir (\e -> execute $ m (\a -> return $ e a))

-- fromBinder
fromBinder :: forall e a. (Stream E () a -> EffKefir e (EffKefir e Unit)) -> EffKefir e (Stream () OT a)
fromBinder f = runFn3 call1Eff "fromBinder" kefir (\e -> execute $ f e)

-- Property
constant :: forall a. a -> EffKefir _ (Property () OT a)
constant = runFn3 call1Eff "constant" kefir


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
}""" :: forall e stream p s a b. Fn2 (stream p s a) (a -> EffKefir e b) (EffKefir e (Unregister e))

onValue :: forall e stream a. (StreamLike stream) => stream _ (HasO _) a -> (a -> EffKefir e _) -> EffKefir e (Unregister e)
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
}""" :: forall e stream s a b. Fn2 (stream s a) (EffKefir e b) (EffKefir e (Unregister e))

onEnd :: forall e stream. (StreamLike stream) => stream _ (HasT _) _ -> (EffKefir e _) -> EffKefir e (Unregister e)
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
}""" :: forall e stream p s a b. Fn3
  {value :: Fn2 Boolean a (Event a), end :: Event a}
  (stream p s a) (Event a -> EffKefir e b) (EffKefir e (Unregister e))

onAny :: forall e stream a. (StreamLike stream) => stream _ _ a -> (Event a -> EffKefir e _) -> EffKefir e (Unregister e)
onAny = runFn3 onAnyImpl {value: mkFn2 Value, end: End}

onLog :: forall stream. (StreamLike stream) => stream _ _ _ -> String -> EffKefir _ Unit
onLog = runFn3 call1Eff "log"

offLog :: forall stream. (StreamLike stream) => stream _ _ _ -> EffKefir _ Unit
offLog = runFn2 call0Eff "offLog"

toProperty :: forall s a. Stream _ s a -> EffKefir _ (Property () s a)
toProperty = runFn2 call0Eff "toProperty"

toPropertyWith :: forall s a. a -> Stream _ s a -> EffKefir _ (Property () s a)
toPropertyWith w s = runFn3 call1Eff "toProperty" s w

changes :: forall s a. Property _ s a -> EffKefir _ (Stream () s a)
changes = runFn2 call0Eff "changes"

withDefault :: forall stream s a. (StreamLike stream) => a -> stream _ s a -> EffKefir _ (Property () s a)
withDefault d s = runFn3 call1Eff "withDefault" s d

-- modify an observable
map :: forall stream s a b. (StreamLike stream) => (a -> b) -> stream _ s a -> EffKefir _ (stream () s b)
map f s = runFn3 call1Eff "map" s f

mapEff :: forall e stream s a b. (StreamLike stream) => (a -> EffKefir e b) -> stream _ s a -> EffKefir e (stream () s b)
mapEff f s = runFn3 call1Eff "map" s (\v -> execute (f v))

filter :: forall stream s a. (StreamLike stream) => (a -> Boolean) -> stream _ s a -> EffKefir _ (stream () s a)
filter f s = runFn3 call1Eff "filter" s f

filterEff :: forall e stream s a. (StreamLike stream) => (a -> EffKefir e Boolean) -> stream _ s a -> EffKefir e (stream () s a)
filterEff f s = runFn3 call1Eff "filter" s (\v -> execute (f v))

take :: forall stream a. (StreamLike stream) => Number -> stream _ _ a -> EffKefir _ (stream () OT a)
take n s = runFn3 call1Eff "take" s n

takeWhile :: forall stream a. (StreamLike stream) => (a -> Boolean) -> stream _ _ a -> EffKefir _ (stream () OT a)
takeWhile f s = runFn3 call1Eff "takeWhile" s f

takeWhileEff :: forall e stream a. (StreamLike stream) => (a -> EffKefir e Boolean) -> stream _ _ a -> EffKefir e (stream () OT a)
takeWhileEff f s = runFn3 call1Eff "takeWhile" s (\v -> execute (f v))

skip :: forall stream s a. (StreamLike stream) => Number -> stream _ s a -> EffKefir _ (stream () s a)
skip n s = runFn3 call1Eff "skip" s n

skipWhile :: forall stream s a. (StreamLike stream) => (a -> Boolean) -> stream _ s a -> EffKefir _ (stream () s a)
skipWhile f s = runFn3 call1Eff "skipWhile" s f

skipWhileEff :: forall e stream s a. (StreamLike stream) => (a -> EffKefir e Boolean) -> stream _ s a -> EffKefir e (stream () s a)
skipWhileEff f s = runFn3 call1Eff "skipWhile" s (\v -> execute (f v))

skipDuplicatesWith :: forall stream s a. (StreamLike stream) => (a -> a -> Boolean) -> stream _ s a -> EffKefir _ (stream () s a)
skipDuplicatesWith f s = runFn3 call1Eff "skipDuplicates" s (mkFn2 f)

skipDuplicates :: forall stream s a. (StreamLike stream, Eq a) => stream _ s a -> EffKefir _ (stream () s a)
skipDuplicates = skipDuplicatesWith (==)

diff1 :: forall stream s a b. (StreamLike stream) => (a -> a -> b) -> stream _ s a -> EffKefir _ (stream () s b)
diff1 f s = runFn3 call1Eff "diff" s (mkFn2 f)

diff :: forall stream s a b. (StreamLike stream) => (a -> a -> b) -> a -> stream _ s a -> EffKefir _ (stream () s b)
diff f a s = runFn4 call2Eff "diff" s (mkFn2 f) a

scan1 :: forall stream s a. (StreamLike stream) => (a -> a -> a) -> stream _ s a -> EffKefir _ (Property () s a)
scan1 f s = runFn3 call1Eff "scan" s (mkFn2 f)

scan :: forall stream s a b. (StreamLike stream) => (b -> a -> b) -> b -> stream _ s a -> EffKefir _ (Property () s b)
scan f a s = runFn4 call2Eff "scan" s (mkFn2 f) a

reduce1 :: forall stream s a. (StreamLike stream) => (a -> a -> a) -> stream _ (HasT s) a -> EffKefir _ (stream () (HasT s) a)
reduce1 f s = runFn3 call1Eff "reduce" s (mkFn2 f)

reduce :: forall stream s a b. (StreamLike stream) => (b -> a -> b) -> b -> stream _ (HasT s) a -> EffKefir _ (stream () (HasT s) b)
reduce f a s = runFn4 call2Eff "reduce" s (mkFn2 f) a

reduceEff1 :: forall e stream s a. (StreamLike stream) => (a -> a -> EffKefir e a) -> stream _ (HasT s) a -> EffKefir e (stream () (HasT s) a)
reduceEff1 f s = runFn3 call1Eff "reduce" s (mkFn2 (\a b -> execute (f a b)))

reduceEff :: forall e stream s a b. (StreamLike stream) => (b -> a -> EffKefir e b) -> b -> stream _ (HasT s) a -> EffKefir e (stream () (HasT s) b)
reduceEff f a s = runFn4 call2Eff "reduce" s (mkFn2 (\a b -> execute (f a b))) a

mapEnd :: forall e stream s a. EffKefir e a -> stream _ (HasT s) a -> EffKefir e (stream () OT a)
mapEnd f s = runFn3 call1Eff "mapEnd" s (wrap f)

skipEnd :: forall stream s a. stream _ (HasT s) a -> EffKefir _ (stream () s a)
skipEnd = runFn2 call0Eff "skipEnd"

type Min = Number
type Max = Number
slidingWindow :: forall stream s a. Min -> Max -> stream _ s a -> EffKefir _ (stream () s [a])
slidingWindow min max s = runFn4 call2Eff "slidingWindow" s max min

bufferWhileWith :: forall stream s a. {flushOnEnd :: Boolean} -> (a -> Boolean) -> stream _ s a -> EffKefir _ (stream () s [a])
bufferWhileWith opts f s = runFn4 call2Eff "bufferWhile" s f opts

bufferWhile :: forall stream s a. (a -> Boolean) -> stream _ s a -> EffKefir _ (stream () s [a])
bufferWhile f s = runFn3 call1Eff "bufferWhile" s f

delay :: forall stream s a. Number -> stream _ s a -> EffKefir _ (stream () s a)
delay w s = runFn3 call1Eff "delay" s w

throttleWith :: forall stream s a. {leading :: Boolean, trailing :: Boolean} -> Number -> stream _ s a -> EffKefir _ (stream () s a)
throttleWith opts w s = runFn4 call2Eff "throttle" s w opts

throttle :: forall stream s a. Number -> stream _ s a -> EffKefir _ (stream () s a)
throttle w s = runFn3 call1Eff "throttle" s w

debounceWith :: forall stream s a. {immediate :: Boolean} -> Number -> stream _ s a -> EffKefir _ (stream () s a)
debounceWith opts w s = runFn4 call2Eff "debounce" s w opts

debounce :: forall stream s a. Number -> stream _ s a -> EffKefir _ (stream () s a)
debounce w s = runFn3 call1Eff "debounce" s w

flatten :: forall stream s a. stream _ s [a] -> EffKefir _ (stream () s a)
flatten = runFn2 call0Eff "flatten"

flattenWith :: forall stream s a b. (a -> [b]) -> stream _ s a -> EffKefir _ (stream () s b)
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
}""" :: forall e stream p s s' a b c. Fn3
  {value :: Fn2 Boolean a (Event a), end :: Event a}
  (stream p s a)
  (Fn2 (Stream E () b) (Event a) (EffKefir e c))
  (EffKefir e (stream () s' b))

withHandler :: forall e stream a b. (StreamLike stream) => stream _ _ a -> (Stream E () b -> Event a -> EffKefir e _) -> EffKefir e (stream () OT b)
withHandler s f = runFn3 withHandlerImpl {value: mkFn2 Value, end: End} s (mkFn2 f)

-- Combine
-- TODO: combine multi stream
combine :: forall streamA streamB a b x. (StreamLike streamA, StreamLike streamB) => streamA _ _ a -> streamB _ _ b -> (a -> b -> x) -> EffKefir _ (Stream () OT x)
combine a b f = runFn4 call2Eff "combine" kefir [unsafeForget a, unsafeForget b] (mkFn2 f)

and :: forall stream s. (StreamLike stream) => [stream _ s Boolean] -> EffKefir _ (Stream () s Boolean)
and = runFn3 call1Eff "and" kefir

or :: forall stream s. (StreamLike stream) => [stream _ s Boolean] -> EffKefir _ (Stream () s Boolean)
or = runFn3 call1Eff "or" kefir

-- TODO: sampledBy multi stream
sampledBy :: forall passive active s a b x. (StreamLike passive, StreamLike active) => passive _ _ a -> active _ s b -> (a -> b -> x) -> EffKefir _ (Stream () s x)
sampledBy pass act f = runFn5 call3Eff "sampledBy" kefir [pass] [act] (mkFn2 f)

-- TODO: zip multi stream
zipWith :: forall streamA streamB a b s x. (StreamLike streamA, StreamLike streamB) => (a -> b -> x) -> streamA _ s a -> streamB _ s b -> EffKefir _ (Stream () s x)
zipWith f a b = runFn4 call2Eff "zip" kefir [unsafeForget a, unsafeForget b] (mkFn2 f)

merge :: forall stream s a. (StreamLike stream) => [stream _ s a] -> EffKefir _ (Stream () s a)
merge = runFn3 call1Eff "merge" kefir

concat :: forall stream s a. (StreamLike stream) => [stream _ s a] -> EffKefir _ (Stream () s a)
concat = runFn3 call1Eff "concat" kefir

pool :: forall e a. EffKefir e (Stream P O a)
pool = runFn2 call0Eff "pool" kefir

plug :: forall stream a. (StreamLike stream) => Stream (HasP _) _ a -> stream _ _ a -> EffKefir _ Unit
plug = runFn3 call1Eff "plug"

unPlug :: forall stream a. (StreamLike stream) => Stream (HasP _) _ a -> stream _ _ a -> EffKefir _ Unit
unPlug = runFn3 call1Eff "unplug"

bus :: forall e a. EffKefir e (Stream EP OT a)
bus = runFn2 call0Eff "bus" kefir

flatMap :: forall stream child a. (StreamLike stream, StreamLike child) => stream _ _ (child _ _ a) -> EffKefir _ (Stream () OT a)
flatMap = runFn2 call0Eff "flatMap"

flatMapLatest :: forall stream child a. (StreamLike stream, StreamLike child) => stream _ _ (child _ _ a) -> EffKefir _ (Stream () OT a)
flatMapLatest = runFn2 call0Eff "flatMapLatest"

flatMapFirst :: forall stream child a. (StreamLike stream, StreamLike child) => stream _ _ (child _ _ a) -> EffKefir _ (Stream () OT a)
flatMapFirst = runFn2 call0Eff "flatMapFirst"

flatMapConcat :: forall stream child a. (StreamLike stream, StreamLike child) => stream _ _ (child _ _ a) -> EffKefir _ (Stream () OT a)
flatMapConcat = runFn2 call0Eff "flatMapConcat"

flatMapWith :: forall e stream child a b. (StreamLike stream, StreamLike child) => stream _ _ a -> (a -> EffKefir e (child _ _ b)) -> EffKefir e (Stream () OT b)
flatMapWith s f = runFn3 call1Eff "flatMap" s (execute <<< f)

flatMapLatestWith :: forall e stream child a b. (StreamLike stream, StreamLike child) => stream _ _ a -> (a -> EffKefir e (child _ _ b)) -> EffKefir e (Stream () OT b)
flatMapLatestWith s f = runFn3 call1Eff "flatMapLatest" s (execute <<< f)

flatMapFirstWith :: forall e stream child a b. (StreamLike stream, StreamLike child) => stream _ _ a -> (a -> EffKefir e (child _ _ b)) -> EffKefir e (Stream () OT b)
flatMapFirstWith s f = runFn3 call1Eff "flatMapFirst" s (execute <<< f)

flatMapConcatWith :: forall e stream child a b. (StreamLike stream, StreamLike child) => stream _ _ a -> (a -> EffKefir e (child _ _ b)) -> EffKefir e (Stream () OT b)
flatMapConcatWith s f = runFn3 call1Eff "flatMapConcat" s (execute <<< f)

foreign import undefined "var undefined = undefined;" :: forall a. a

flatMapConcurLimit :: forall stream child a. (StreamLike stream, StreamLike child) => Number -> stream _ _ (child _ _ a) -> EffKefir _ (Stream () OT a)
flatMapConcurLimit l s = runFn4 call2Eff "flatMapConcurLimit" s undefined l

flatMapConcurLimitWith :: forall e stream child a b. (StreamLike stream, StreamLike child) => Number -> stream _ _ a -> (a -> EffKefir e (child _ _ b)) -> EffKefir e (Stream () OT b)
flatMapConcurLimitWith l s f = runFn4 call2Eff "flatMapConcurLimit" s (execute <<< f) l

-- combine two observables
filterBy :: forall stream filter s a. (StreamLike stream, StreamLike filter) => stream _ s a -> filter _ _ Boolean -> EffKefir _ (stream () s a)
filterBy v f = runFn3 call1Eff "filterBy" v f

takeWhileBy :: forall stream filter a. (StreamLike stream, StreamLike filter) => stream _ _ a -> filter _ _ Boolean -> EffKefir _ (stream () OT a)
takeWhileBy v f = runFn3 call1Eff "takeWhileBy" v f

skipWhileBy :: forall stream filter s a. (StreamLike stream, StreamLike filter) => stream _ s a -> filter _ _ Boolean -> EffKefir _ (stream () s a)
skipWhileBy v f = runFn3 call1Eff "skipWhileBy" v f

skipUntilBy :: forall stream filter s a. (StreamLike stream, StreamLike filter) => stream _ s a -> filter _ _ _ -> EffKefir _ (stream () s a)
skipUntilBy v f = runFn3 call1Eff "skipUntilBy" v f

takeUntilBy :: forall stream filter a. (StreamLike stream, StreamLike filter) => stream _ _ a -> filter _ _ _ -> EffKefir _ (stream () OT a)
takeUntilBy v f = runFn3 call1Eff "takeUntilBy" v f

bufferBy :: forall stream filter s a. (StreamLike stream, StreamLike filter) => stream _ s a -> filter _ _ _ -> EffKefir _ (stream () s [a])
bufferBy v f = runFn3 call1Eff "bufferBy" v f

bufferWhileBy :: forall stream filter s a. (StreamLike stream, StreamLike filter) => stream _ s a -> filter _ _ Boolean -> EffKefir _ (stream () s [a])
bufferWhileBy v f = runFn3 call1Eff "bufferWhileBy" v f

awaiting :: forall on off s. (StreamLike on, StreamLike off) => on _ s _ -> off _ s _ -> EffKefir _ (Property () s Boolean)
awaiting v f = runFn3 call1Eff "awaiting" v f
