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
  , onLog, onLogWith, offLog

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

foreign import execute """
  function execute(m){
    return m();
  }""" :: forall e a. Eff e a -> a

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
foreign import emitterImpl """
function emitterImpl(kefir){
  return function EmitterEff(){
    return kefir.emitter();
  }
}""" :: forall a. a

emitter :: EffKefir _ (Stream E OT _)
emitter = emitterImpl kefir

foreign import emitImpl """
function emitImpl(stream, value){
  return function EmitEff(){
    stream.emit(value);
  }
}""" :: forall a. a

emit :: forall a. Stream (HasE _) _ a -> a -> EffKefir _ Unit
emit s a = runFn2 emitImpl s a

foreign import endImpl """
function endImpl(stream){
  return function EndEff(){
    stream.end();
  }
}""" :: forall a. a

end :: Stream (HasE _) _ _ -> EffKefir _ Unit
end = endImpl

foreign import emitAsyncImpl """
function emitAsyncImpl(stream, value){
  return function EmitAsyncEff(){
    setTimeout(function(){stream.emit(value);}, 0);
  }
}""" :: forall a. a

emitAsync :: forall a. Stream (HasE _) _ a -> a -> EffKefir _ Unit
emitAsync s a = runFn2 emitAsyncImpl s a

foreign import endAsyncImpl """
function endAsyncImpl(stream){
  return function EndAsyncEff(){
    setTimeout(function(){stream.end();}, 0);
  }
}""" :: forall a. a

endAsync :: Stream (HasE _) _ _ -> EffKefir _ Unit
endAsync = endAsyncImpl

-- never
foreign import neverImpl """
function neverImpl(kefir){
  return function NeverEff(){
    return kefir.never();
  }
}""" :: forall a. a

never :: EffKefir _ (Stream () T _)
never = neverImpl kefir

-- later
foreign import laterImpl """
function laterImpl(kefir, time, value){
  return function LaterEff(){
    return kefir['later'](time, value);
  }
}""" :: forall a. a

later :: forall a. Number -> a -> EffKefir _ (Stream () OT a)
later t a = runFn3 laterImpl kefir t a

-- Interval
foreign import intervalImpl """
function intervalImpl(kefir, time, value){
  return function IntervalEff(){
    return kefir.interval(time, value);
  }
}""" :: forall a. a

interval :: forall a. Number -> a -> EffKefir _ (Stream () O a)
interval t a = runFn3 intervalImpl kefir t a

-- sequentially
foreign import sequentiallyImpl """
function sequentiallyImpl(kefir, time, value){
  return function SequentiallyEff(){
    return kefir['sequentially'](time, value);
  }
}""" :: forall a. a

sequentially :: forall a. Number -> [a] -> EffKefir _ (Stream () OT a)
sequentially t a = runFn3 sequentiallyImpl kefir t a

-- repeatedly
foreign import repeatedlyImpl """
function repeatedlyImpl(kefir, time, value){
  return function RepeatedlyEff(){
    return kefir['repeatedly'](time, value);
  }
}""" :: forall a. a

repeatedly :: forall a. Number -> [a] -> EffKefir _ (Stream () O a)
repeatedly t a = runFn3 repeatedlyImpl kefir t a

-- fromPoll
foreign import fromPollImpl """
function fromPollImpl(kefir, time, func){
  return function FromPollEff(){
    return kefir['fromPoll'](time, func);
  }
}""" :: forall a. a

fromPoll :: forall e a. Number -> EffKefir e a -> EffKefir e (Stream () O a)
fromPoll t a = runFn3 fromPollImpl kefir t a

-- withinterval
foreign import withIntervalImpl """
function withIntervalImpl(kefir, interval, cb){
  return function WithIntervalEff(){
    return kefir['withInterval'](interval, function(e){cb(e)();});
  }
}""" :: forall a. a

withInterval :: forall e a. Number -> (Stream E () a -> EffKefir e Unit) -> EffKefir e (Stream () OT a)
withInterval t a = runFn3 withIntervalImpl kefir t a

-- fromCallback
foreign import fromCallbackImpl """
function fromCallbackImpl(kefir, fn){
  return function FroCallbackEff(){
    return kefir.fromCallback(function(cb){
      function callback(a){
        return function (){
          cb(a);
        }
      }
      fn(callback)();
    });
  }
}""" :: forall a. a

fromCallback :: forall e a. ((a -> EffKefir e Unit) -> EffKefir e Unit) -> EffKefir e (Stream () OT a)
fromCallback f = runFn2 fromCallbackImpl kefir f

-- fromBinder
foreign import fromBinderImpl """
function fromBinderImpl(kefir, binder){
  return function FromBinderEff(){
    return kefir.fromBinder(function(emitter){
      return binder(emitter)();
    });
  }
}""" :: forall a. a

fromBinder :: forall e a. (Stream E () a -> EffKefir e (EffKefir e Unit)) -> EffKefir e (Stream () OT a)
fromBinder f = runFn2 fromBinderImpl kefir f

-- Property
foreign import constantImpl """
function constantImpl(kefir, a){
  return function CinstantEff(){
    return kefir.constant(a);
  }
}""" :: forall a. a

constant :: forall a. a -> EffKefir _ (Property () OT a)
constant a = runFn2 constantImpl kefir a


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
onValue s f = runFn2 onValueImpl s f

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
onEnd s f = runFn2 onEndImpl s f

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
onAny s f = runFn3 onAnyImpl {value: mkFn2 Value, end: End} s f

foreign import onLogImpl """
function onLogImpl (sream) {
  return function OnLogEff(){
    stream.log();
  }
}""" :: forall a. a

onLog :: forall stream. (StreamLike stream) => stream _ _ _ -> EffKefir _ Unit
onLog = onLogImpl

foreign import onLogWithImpl """
function onLogWithImpl (sream, str) {
  return function OnLogWithEff(){
    stream.log(str);
  }
}""" :: forall a. a

onLogWith :: forall stream. (StreamLike stream) => stream _ _ _ -> String -> EffKefir _ Unit
onLogWith s n = runFn2 onLogWithImpl s n

foreign import offLogImpl """
function offLogImpl (sream) {
  return function OffLogEff(){
    stream.offLog();
  }
}""" :: forall a. a


offLog :: forall stream. (StreamLike stream) => stream _ _ _ -> EffKefir _ Unit
offLog s = offLogImpl s

foreign import toPropertyImpl """
function toPropertyImpl(stream){
  return function ToPropertyEff(){
    return stream['toProperty']();
  }
}""" :: forall a. a

toProperty :: forall s a. Stream _ s a -> EffKefir _ (Property () s a)
toProperty = toPropertyImpl

foreign import toPropertyWithImpl """
function toPropertyWithImpl(stream, v){
  return function ToPropertyWithEff(){
    return stream['toProperty'](v);
  }
}""" :: forall a. a

toPropertyWith :: forall s a. a -> Stream _ s a -> EffKefir _ (Property () s a)
toPropertyWith w s = runFn2 toPropertyWithImpl s w

foreign import changesImpl """
function changesImpl(stream){
  return function ChangesEff(){
    return stream['changes']();
  }
}""" :: forall a. a

changes :: forall s a. Property _ s a -> EffKefir _ (Stream () s a)
changes = changesImpl

foreign import withDefaultImpl """
function withDefaultImpl(stream, v){
  return function WithDefaultEff(){
    return stream['withDefault'](v);
  }
}""" :: forall a. a

withDefault :: forall stream s a. (StreamLike stream) => a -> stream _ s a -> EffKefir _ (Property () s a)
withDefault d s = runFn2 withDefaultImpl s d

foreign import mapImpl """
function mapImpl(fn, stream){
  return function MapEff(){
    return stream.map(fn);
  }
}""" :: forall a. a

-- modify an observable
map :: forall stream s a b. (StreamLike stream) => (a -> b) -> stream _ s a -> EffKefir _ (stream () s b)
map f s = runFn2 mapImpl f s

foreign import mapEffImpl """
function mapEffImpl(fn, stream){
  return function MapEffEff(){
    return stream.map(function(a){return fn(a)();});
  }
}""" :: forall a. a

mapEff :: forall e stream s a b. (StreamLike stream) => (a -> EffKefir e b) -> stream _ s a -> EffKefir e (stream () s b)
mapEff f s = runFn2 mapEffImpl f s

foreign import filterImpl """
function filterImpl(fn, stream){
  return function FilterEff(){
    return stream.filter(fn);
  }
}""" :: forall a. a

filter :: forall stream s a. (StreamLike stream) => (a -> Boolean) -> stream _ s a -> EffKefir _ (stream () s a)
filter f s = runFn2 filterImpl f s

foreign import filterEffImpl """
function filterEffImpl(fn, stream){
  return function FilterEffEff(){
    return stream.filter(function(a){fn(a)();});
  }
}""" :: forall a. a

filterEff :: forall e stream s a. (StreamLike stream) => (a -> EffKefir e Boolean) -> stream _ s a -> EffKefir e (stream () s a)
filterEff f s = runFn2 filterEffImpl f s

foreign import takeImpl """
function takeImpl(n, stream){
  return function TakeEff(){
    return stream['take'](n);
  }
}""" :: forall a. a

take :: forall stream a. (StreamLike stream) => Number -> stream _ _ a -> EffKefir _ (stream () OT a)
take n s = runFn2 takeImpl n s

foreign import takeWhileImpl """
function takeWhileImpl(f, stream){
  return function TakeWhileEff(){
    return stream['takeWhile'](f);
  }
}""" :: forall a. a

takeWhile :: forall stream a. (StreamLike stream) => (a -> Boolean) -> stream _ _ a -> EffKefir _ (stream () OT a)
takeWhile f s = runFn2 takeWhileImpl f s

foreign import takeWhileEffImpl """
function takeWhileEffImpl(f, stream){
  return function TakeWhileEffEff(){
    return stream['takeWhile'](function(a){return f(a)();});
  }
}""" :: forall a. a

takeWhileEff :: forall e stream a. (StreamLike stream) => (a -> EffKefir e Boolean) -> stream _ _ a -> EffKefir e (stream () OT a)
takeWhileEff f s = runFn2 takeWhileEffImpl f s

foreign import skipImpl """
function skipImpl(n, stream){
  return function SkipEff(){
    return stream['skip'](n);
  }
}""" :: forall a. a

skip :: forall stream s a. (StreamLike stream) => Number -> stream _ s a -> EffKefir _ (stream () s a)
skip n s = runFn2 skipImpl n s

foreign import skipWhileImpl """
function skipWhileImpl(f, stream){
  return function SkipWhileEff(){
    return stream['skipWhile'](f);
  }
}""" :: forall a. a

skipWhile :: forall stream s a. (StreamLike stream) => (a -> Boolean) -> stream _ s a -> EffKefir _ (stream () s a)
skipWhile f s = runFn2 skipWhileImpl f s

foreign import skipWhileEffImpl """
function skipWhileEffImpl(f, stream){
  return function SkipWhileEffEff(){
    return stream['skipWhile'](function(a){return f(a)();});
  }
}""" :: forall a. a

skipWhileEff :: forall e stream s a. (StreamLike stream) => (a -> EffKefir e Boolean) -> stream _ s a -> EffKefir e (stream () s a)
skipWhileEff f s = runFn2 skipWhileEffImpl f s

foreign import skipDuplicatesWithImpl """
function skipDuplicatesWithImpl(f, stream){
  return function SkipDuplicatesWithEff(){
    return stream['skipDuplicates'](f);
  }
}""" :: forall a. a

skipDuplicatesWith :: forall stream s a. (StreamLike stream) => (a -> a -> Boolean) -> stream _ s a -> EffKefir _ (stream () s a)
skipDuplicatesWith f s = runFn2 skipDuplicatesWithImpl (mkFn2 f) s

skipDuplicates :: forall stream s a. (StreamLike stream, Eq a) => stream _ s a -> EffKefir _ (stream () s a)
skipDuplicates = skipDuplicatesWith (==)

foreign import diff1Impl """
function diff1Impl(f, stream){
  return function Diff1Eff(){
    return stream['diff'](f);
  }
}""" :: forall a. a

diff1 :: forall stream s a b. (StreamLike stream) => (a -> a -> b) -> stream _ s a -> EffKefir _ (stream () s b)
diff1 f s = runFn2 diff1Impl (mkFn2 f) s

foreign import diffImpl """
function diffImpl(f, a, stream){
  return function DiffEff(){
    return stream['diff'](f, a);
  }
}""" :: forall a. a

diff :: forall stream s a b. (StreamLike stream) => (a -> a -> b) -> a -> stream _ s a -> EffKefir _ (stream () s b)
diff f a s = runFn3 diffImpl (mkFn2 f) a s

foreign import scan1Impl """
function scan1Impl(f, stream){
  return function Scan1Eff(){
    return stream['scan'](f);
  }
}""" :: forall a. a

scan1 :: forall stream s a. (StreamLike stream) => (a -> a -> a) -> stream _ s a -> EffKefir _ (Property () s a)
scan1 f s = runFn2 scan1Impl (mkFn2 f) s

foreign import scanImpl """
function scanImpl(f, a, stream){
  return function ScanEff(){
    return stream['scan'](f, a);
  }
}""" :: forall a. a

scan :: forall stream s a b. (StreamLike stream) => (b -> a -> b) -> b -> stream _ s a -> EffKefir _ (Property () s b)
scan f a s = runFn3 scanImpl (mkFn2 f) a s

foreign import reduce1Impl """
function reduce1Impl(f, stream){
  return function Reduce1Eff(){
    return stream.reduce(f);
  }
}""" :: forall a. a

reduce1 :: forall stream s a. (StreamLike stream) => (a -> a -> a) -> stream _ (HasT s) a -> EffKefir _ (stream () (HasT s) a)
reduce1 f s = runFn2 reduce1Impl (mkFn2 f) s

foreign import reduceImpl """
function reduceImpl(f, a, stream){
  return function ReduceEff(){
    return stream.reduce(f, a);
  }
}""" :: forall a. a

reduce :: forall stream s a b. (StreamLike stream) => (b -> a -> b) -> b -> stream _ (HasT s) a -> EffKefir _ (stream () (HasT s) b)
reduce f a s = runFn3 reduceImpl (mkFn2 f) a s

foreign import reduceEff1Impl """
function reduceEff1Impl(f, stream){
  return function ReduceEff1Eff(){
    return stream.reduce(function(a,b){return f(a)(b)();});
  }
}""" :: forall a. a

reduceEff1 :: forall e stream s a. (StreamLike stream) => (a -> a -> EffKefir e a) -> stream _ (HasT s) a -> EffKefir e (stream () (HasT s) a)
reduceEff1 f s = runFn2 reduceEff1Impl f s

foreign import reduceEffImpl """
function reduceEffImpl(f, a, stream){
  return function ReduceEffEff(){
    return stream.reduce(function(a,b){return f(a)(b)();}, a);
  }
}""" :: forall a. a

reduceEff :: forall e stream s a b. (StreamLike stream) => (b -> a -> EffKefir e b) -> b -> stream _ (HasT s) a -> EffKefir e (stream () (HasT s) b)
reduceEff f a s = runFn3 reduceEffImpl f a s

foreign import mapEndImpl """
function mapEndImpl(f, stream){
  return function MapEndEff(){
    return stream['mapEnd'](function(){return f();});
  }
}""" :: forall a. a

mapEnd :: forall e stream s a. EffKefir e a -> stream _ (HasT s) a -> EffKefir e (stream () OT a)
mapEnd f s = runFn2 mapEndImpl f s

foreign import skipEndImpl """
function skipEndImpl(stream){
  return function SkipEndEff(){
    return stream['skipEnd']();
  }
}""" :: forall a. a

skipEnd :: forall stream s a. stream _ (HasT s) a -> EffKefir _ (stream () s a)
skipEnd = skipEndImpl

foreign import slidingWindowImpl """
function slidingWindowImpl(min, max, stream){
  return function SlidingWindowEff(){
    return stream['slidingWindow'](max, min);
  }
}""" :: forall a. a

type Min = Number
type Max = Number
slidingWindow :: forall stream s a. Min -> Max -> stream _ s a -> EffKefir _ (stream () s [a])
slidingWindow min max s = runFn3 slidingWindowImpl min max s

foreign import bufferWhileImpl """
function bufferWhileImpl(f, stream){
  return function BufferWhileEff(){
    return stream['bufferWhile'](f);
  }
}""" :: forall a. a

bufferWhile :: forall stream s a. (a -> Boolean) -> stream _ s a -> EffKefir _ (stream () s [a])
bufferWhile f s = runFn2 bufferWhileImpl f s

foreign import bufferWhileWithImpl """
function bufferWhileWithImpl(opts, f, stream){
  return function BufferWhileEff(){
    return stream['bufferWhile'](f, {'flushOnEnd': opts.flushOnEnd});
  }
}""" :: forall a. a

bufferWhileWith :: forall stream s a. {flushOnEnd :: Boolean} -> (a -> Boolean) -> stream _ s a -> EffKefir _ (stream () s [a])
bufferWhileWith opts f s = runFn3 bufferWhileWithImpl opts f s

foreign import delayImpl """
function delayImpl(d, stream){
  return function DelayEff(){
    return stream['delay'](d);
  }
}""" :: forall a. a

delay :: forall stream s a. Number -> stream _ s a -> EffKefir _ (stream () s a)
delay w s = runFn2 delayImpl w s

foreign import throttleImpl """
function throttleImpl(d, stream){
  return function ThrottleEff(){
    return stream['throttle'](d);
  }
}""" :: forall a. a

throttle :: forall stream s a. Number -> stream _ s a -> EffKefir _ (stream () s a)
throttle d s = runFn2 throttleImpl d s

foreign import throttleWithImpl """
function throttleWithImpl(opts, d, stream){
  return function ThrottleWithEff(){
    var o = {'leading': opts.leading, 'trailing': opts.trailing};
    return stream['throttle'](d, o);
  }
}""" :: forall a. a

throttleWith :: forall stream s a. {leading :: Boolean, trailing :: Boolean} -> Number -> stream _ s a -> EffKefir _ (stream () s a)
throttleWith opts d s = runFn3 throttleWithImpl opts d s

foreign import debounceImpl """
function debounceImpl(d, stream){
  return function DebounceEff(){
    return stream['debounce'](d);
  }
}""" :: forall a. a

debounce :: forall stream s a. Number -> stream _ s a -> EffKefir _ (stream () s a)
debounce d s = runFn2 debounceImpl d s

foreign import debounceWithImpl """
function debounceWithImpl(opts, d, stream){
  return function DebounceWithEff(){
    return stream['debounce'](d, {'immediate': opts.immediate});
  }
}""" :: forall a. a

debounceWith :: forall stream s a. {immediate :: Boolean} -> Number -> stream _ s a -> EffKefir _ (stream () s a)
debounceWith opts d s = runFn3 debounceWithImpl opts d s

foreign import flattenImpl """
function flattenImpl(stream){
  return function FlattenEff(){
    return stream['flatten']();
  }
}""" :: forall a. a

flatten :: forall stream s a. stream _ s [a] -> EffKefir _ (stream () s a)
flatten = flattenImpl

foreign import flattenWithImpl """
function flattenWithImpl(f, stream){
  return function FlattenWithEff(){
    return stream['flatten'](f);
  }
}""" :: forall a. a

flattenWith :: forall stream s a b. (a -> [b]) -> stream _ s a -> EffKefir _ (stream () s b)
flattenWith f s = runFn2 flattenWithImpl f s

foreign import withHandlerImpl """
function withHandlerImpl(cnsts, src, fun){
  return function withHandlerImplEff(){
    return src['withHandler'](function(emitter, ev){
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
foreign import combineImpl """
function combineImpl(a, b, f) {
  return function CombineEff(){
    return a.combine(b, f);
  }
}""" :: forall a. a

combine :: forall streamA streamB a b x. (StreamLike streamA, StreamLike streamB) => streamA _ _ a -> streamB _ _ b -> (a -> b -> x) -> EffKefir _ (Stream () OT x)
combine a b f = runFn3 combineImpl a b (mkFn2 f)

foreign import andImpl """
function andImpl(kefir, os) {
  return function AndEff(){
    return kefir.and(os);
  }
}""" :: forall a. a

and :: forall stream s. (StreamLike stream) => [stream _ s Boolean] -> EffKefir _ (Stream () s Boolean)
and os = runFn2 andImpl kefir os

foreign import orImpl """
function orImpl(kefir, os) {
  return function OrEff(){
    return kefir.or(os);
  }
}""" :: forall a. a

or :: forall stream s. (StreamLike stream) => [stream _ s Boolean] -> EffKefir _ (Stream () s Boolean)
or os = runFn2 orImpl kefir os

-- TODO: sampledBy multi stream
foreign import sampledByImpl """
function sampledByImpl(pas, act, fn) {
  return function SampledByEff(){
    return pas.sampledBy(act, fn);
  }
}""" :: forall a. a

sampledBy :: forall passive active s a b x. (StreamLike passive, StreamLike active) => passive _ _ a -> active _ s b -> (a -> b -> x) -> EffKefir _ (Stream () s x)
sampledBy pass act f = runFn3 sampledByImpl pass act (mkFn2 f)

-- TODO: zip multi stream
foreign import zipWithImpl """
function zipWithImpl(f, a, b) {
  return function ZipWithEff(){
    return a.zip(b, f);
  }
}""" :: forall a. a

zipWith :: forall streamA streamB a b s x. (StreamLike streamA, StreamLike streamB) => (a -> b -> x) -> streamA _ s a -> streamB _ s b -> EffKefir _ (Stream () s x)
zipWith f a b = runFn3 zipWithImpl (mkFn2 f) a b

foreign import mergeImpl """
function mergeImpl(kefir, os) {
  return function MergeEff(){
    return kefir.merge(os);
  }
}""" :: forall a. a

merge :: forall stream s a. (StreamLike stream) => [stream _ s a] -> EffKefir _ (Stream () s a)
merge os = runFn2 mergeImpl kefir os

foreign import concatImpl """
function concatImpl(kefir, os) {
  return function ConcatEff(){
    return kefir['concat'](os);
  }
}""" :: forall a. a

concat :: forall stream s a. (StreamLike stream) => [stream _ s a] -> EffKefir _ (Stream () s a)
concat os = runFn2 concatImpl kefir os

foreign import poolImpl """
function poolImpl(kefir) {
  return function PoolEff(){
    return kefir.pool();
  }
}""" :: forall a. a

pool :: forall e a. EffKefir e (Stream P O a)
pool = poolImpl kefir

foreign import plugImpl """
function plugImpl(pool, stream){
  return function PlugEff(){
    pool.plug(stream);
  }
}""" :: forall a. a

plug :: forall stream a. (StreamLike stream) => Stream (HasP _) _ a -> stream _ _ a -> EffKefir _ Unit
plug p s = runFn2 plugImpl p s

foreign import unPlugImpl """
function unPlugImpl(pool, stream){
  return function UnPlugEff(){
    pool.unplug(stream);
  }
}""" :: forall a. a

unPlug :: forall stream a. (StreamLike stream) => Stream (HasP _) _ a -> stream _ _ a -> EffKefir _ Unit
unPlug p s = runFn2 unPlugImpl p s

foreign import busImpl """
function busImpl(kefir) {
  return function BusEff(){
    return kefir.bus();
  }
}""" :: forall a. a

bus :: forall e a. EffKefir e (Stream EP OT a)
bus = busImpl kefir

foreign import flatMapImpl """
function flatMapImpl(stream) {
  return function FlatMapEff(){
    return stream.flatMap();
  }
}""" :: forall a. a

flatMap :: forall stream child a. (StreamLike stream, StreamLike child) => stream _ _ (child _ _ a) -> EffKefir _ (Stream () OT a)
flatMap = flatMapImpl

foreign import flatMapLatestImpl """
function flatMapLatestImpl(stream) {
  return function FlatMapLatestEff(){
    return stream.flatMapLatest();
  }
}""" :: forall a. a

flatMapLatest :: forall stream child a. (StreamLike stream, StreamLike child) => stream _ _ (child _ _ a) -> EffKefir _ (Stream () OT a)
flatMapLatest = flatMapLatestImpl

foreign import flatMapFirstImpl """
function flatMapFirstImpl(stream) {
  return function FlatMapFirstEff(){
    return stream.flatMapFirst();
  }
}""" :: forall a. a

flatMapFirst :: forall stream child a. (StreamLike stream, StreamLike child) => stream _ _ (child _ _ a) -> EffKefir _ (Stream () OT a)
flatMapFirst = flatMapFirstImpl

foreign import flatMapConcatImpl """
function flatMapConcatImpl(stream) {
  return function FlatMapConcatEff(){
    return stream.flatMapConcat();
  }
}""" :: forall a. a

flatMapConcat :: forall stream child a. (StreamLike stream, StreamLike child) => stream _ _ (child _ _ a) -> EffKefir _ (Stream () OT a)
flatMapConcat = flatMapConcatImpl

foreign import flatMapWithImpl """
function flatMapWithImpl(stream, fn) {
  return function FlatMapWithEff(){
    return stream.flatMap(function(a){return fn(a)()});
  }
}""" :: forall a. a

flatMapWith :: forall e stream child a b. (StreamLike stream, StreamLike child) => stream _ _ a -> (a -> EffKefir e (child _ _ b)) -> EffKefir e (Stream () OT b)
flatMapWith s f = runFn2 flatMapWithImpl s f

foreign import flatMapLatestWithImpl """
function flatMapLatestWithImpl(stream, fn) {
  return function FlatMapLatestWithEff(){
    return stream.flatMapLatest(function(a){return fn(a)()});
  }
}""" :: forall a. a

flatMapLatestWith :: forall e stream child a b. (StreamLike stream, StreamLike child) => stream _ _ a -> (a -> EffKefir e (child _ _ b)) -> EffKefir e (Stream () OT b)
flatMapLatestWith s f = runFn2 flatMapLatestWithImpl s f

foreign import flatMapFirstWithImpl """
function flatMapFirstWithImpl(stream, fn) {
  return function FlatMapFirstWithEff(){
    return stream.flatMapFirst(function(a){return fn(a)()});
  }
}""" :: forall a. a

flatMapFirstWith :: forall e stream child a b. (StreamLike stream, StreamLike child) => stream _ _ a -> (a -> EffKefir e (child _ _ b)) -> EffKefir e (Stream () OT b)
flatMapFirstWith s f = runFn2 flatMapFirstWithImpl s f

foreign import flatMapConcatWithImpl """
function flatMapConcatWithImpl(stream, fn) {
  return function FlatMapConcatWithEff(){
    return stream.flatMapConcat(function(a){return fn(a)()});
  }
}""" :: forall a. a

flatMapConcatWith :: forall e stream child a b. (StreamLike stream, StreamLike child) => stream _ _ a -> (a -> EffKefir e (child _ _ b)) -> EffKefir e (Stream () OT b)
flatMapConcatWith s f = runFn2 flatMapConcatWithImpl s f

foreign import flatMapConcurLimitImpl """
function flatMapConcurLimitImpl(lim, stream) {
  return function FlatMapConcurLimitEff(){
    return stream.flatMapConcurLimit(undefined, lim);
  }
}""" :: forall a. a

flatMapConcurLimit :: forall stream child a. (StreamLike stream, StreamLike child) => Number -> stream _ _ (child _ _ a) -> EffKefir _ (Stream () OT a)
flatMapConcurLimit l s = runFn2 flatMapConcurLimitImpl l s

foreign import flatMapConcurLimitWithImpl """
function flatMapConcurLimitWithImpl(lim, stream, fn) {
  return function FlatMapConcurLimitWithEff(){
    return stream.flatMapConcurLimit(function(a){return fn(a)()}, lim);
  }
}""" :: forall a. a

flatMapConcurLimitWith :: forall e stream child a b. (StreamLike stream, StreamLike child) => Number -> stream _ _ a -> (a -> EffKefir e (child _ _ b)) -> EffKefir e (Stream () OT b)
flatMapConcurLimitWith l s f = runFn3 flatMapConcurLimitWithImpl l s f

-- combine two observables
foreign import filterByImpl """
function filterByImpl(s, f){
  return function FilterEff(){
    return s['filterBy'](f);
  }
}""" :: forall a. a

filterBy :: forall stream filter s a. (StreamLike stream, StreamLike filter) => stream _ s a -> filter _ _ Boolean -> EffKefir _ (stream () s a)
filterBy s f = runFn2 filterByImpl s f

foreign import takeWhileByImpl """
function takeWhileByImpl(s, f){
  return function TakeWhileByEff(){
    return s['takeWhileBy'](f);
  }
}""" :: forall a. a

takeWhileBy :: forall stream filter a. (StreamLike stream, StreamLike filter) => stream _ _ a -> filter _ _ Boolean -> EffKefir _ (stream () OT a)
takeWhileBy s f = runFn2 takeWhileByImpl s f

foreign import skipWhileByImpl """
function skipWhileByImpl(s, f){
  return function SkipWhileByEff(){
    return s['skipWhileBy'](f);
  }
}""" :: forall a. a

skipWhileBy :: forall stream filter s a. (StreamLike stream, StreamLike filter) => stream _ s a -> filter _ _ Boolean -> EffKefir _ (stream () s a)
skipWhileBy s f = runFn2 skipWhileByImpl s f

foreign import skipUntilByImpl """
function skipUntilByImpl(s, f){
  return function SkipUntilByEff(){
    return s['skipUntilBy'](f);
  }
}""" :: forall a. a

skipUntilBy :: forall stream filter s a. (StreamLike stream, StreamLike filter) => stream _ s a -> filter _ _ _ -> EffKefir _ (stream () s a)
skipUntilBy s f = runFn2 skipUntilByImpl s f

foreign import takeUntilByImpl """
function takeUntilByImpl(s, f){
  return function TakeUntilByEff(){
    return s['takeUntilBy'](f);
  }
}""" :: forall a. a

takeUntilBy :: forall stream filter a. (StreamLike stream, StreamLike filter) => stream _ _ a -> filter _ _ _ -> EffKefir _ (stream () OT a)
takeUntilBy s f = runFn2 takeUntilByImpl s f

foreign import bufferByImpl """
function bufferByImpl(s, f){
  return function BufferByImpl(){
    return s['bufferBy'](f);
  }
}""" :: forall a. a

bufferBy :: forall stream filter s a. (StreamLike stream, StreamLike filter) => stream _ s a -> filter _ _ _ -> EffKefir _ (stream () s [a])
bufferBy s f = runFn2 bufferByImpl s f

foreign import bufferWhileByImpl """
function bufferWhileByImpl(s, f){
  return function BufferWhileByImpl(){
    return s['bufferWhileBy'](f);
  }
}""" :: forall a. a

bufferWhileBy :: forall stream filter s a. (StreamLike stream, StreamLike filter) => stream _ s a -> filter _ _ Boolean -> EffKefir _ (stream () s [a])
bufferWhileBy s f = runFn2 bufferWhileByImpl s f

foreign import awaitingImpl """
function awaitingImpl(s, f){
  return function AwaitingEff(){
    return s.awaiting(f);
  }
}""" :: forall a. a

awaiting :: forall on off s. (StreamLike on, StreamLike off) => on _ s _ -> off _ s _ -> EffKefir _ (Property () s Boolean)
awaiting s f = runFn2 awaitingImpl s f
