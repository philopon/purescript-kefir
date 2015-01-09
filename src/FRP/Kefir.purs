module FRP.Kefir
  ( Kefir(), Stream(), Property(), EffKefir()

  , Terminable(), Observable(), HasError()
  , Emittable(), Pluggable()

  , Obs(), Err(), End()
  , ObsEnd(), ObsErr(), ErrEnd()
  , All()

  , Emit(), Plug(), EmitPlug()

  , Unregister()
  , onEnd
  , onValue
  , onError
  , emit, error, end
  , emitAsync, errorAsync, endAsync
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
  , fromNodeCallback
  , fromBinder

  , constant
  , constantError

  , toProperty, toPropertyWith
  , changes

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
  , valuesToErrors
  , errorsToValues
  , mapErrors, mapErrorsEff
  , filterErrors, filterErrorsEff
  , skipErrors
  , skipValues
  , endOnError

  , combine
  , and, or
  , Active(), Passive(), sampledBy
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
  , On(), Off(), awaiting

  , unsafeGlobalize
  ) where

import Control.Monad.Eff
import FRP.Kefir.Foreign
import Data.Maybe
import Data.Maybe.Unsafe
import Data.Either
import Data.Function

foreign import data Kefir    :: !
foreign import data Stream   :: # * -> # * -> * -> * -> *
type Property = Stream

type EffKefir e = Eff (kefir :: Kefir | e)

-- Stream

foreign import execute """
  function execute(m){
    return m();
  }""" :: forall e a. Eff e a -> a

foreign import data Terminable :: *
foreign import data Observable :: *
foreign import data HasError   :: *

foreign import data Emittable  :: *
foreign import data Pluggable  :: *

type Obs s = (observable :: Observable | s)
type End s = (terminable :: Terminable | s)
type Err s = (hasError   :: HasError   | s)

type ObsEnd s = (observable :: Observable, terminable :: Terminable | s)
type ObsErr s = (observable :: Observable, hasError :: HasError | s)
type ErrEnd s = (hasError :: HasError, terminable :: Terminable | s)
type All    s = (hasError :: HasError, observable :: Observable, terminable :: Terminable | s)

type Emit s = (emittable  :: Emittable | s)
type Plug s = (pluggable  :: Pluggable | s)
type EmitPlug s = (emittable :: Emittable, pluggable :: Pluggable | s)

foreign import forgetImpl """
function forgetImpl(stream){
  return stream;
}""" :: forall str str' s s' a b. str s a -> str' s' b

asStream :: forall s e a. Stream _ s e a -> Stream _ s e a
asStream = forgetImpl

forget :: forall e a. Stream _ _ e a -> Stream _ _ e a
forget = forgetImpl

unsafeForget :: Stream _ _ _ _ -> Stream _ _ _ _
unsafeForget = forgetImpl

unsafeGlobalize :: forall p s e a. EffKefir _ (Stream p s e a) -> Stream p s e a
unsafeGlobalize = execute

-- Emitter
foreign import emitterImpl """
function emitterImpl(kefir){
  return function EmitterEff(){
    return kefir.emitter();
  }
}""" :: forall a. a

emitter :: EffKefir _ (Stream (Emit()) (All()) _ _)
emitter = emitterImpl kefir

foreign import emitImpl """
function emitImpl(stream, value){
  return function EmitEff(){
    stream.emit(value);
  }
}""" :: forall a. a

emit :: forall a. Stream (Emit _) (Obs _) _ a -> a -> EffKefir _ Unit
emit s a = runFn2 emitImpl s a

foreign import errorImpl """
function errorImpl(stream, value){
  return function ErrorEff(){
    stream.error(value);
  }
}""" :: forall a. a

error :: forall e. Stream (Emit _) (Err _) e _ -> e -> EffKefir _ Unit
error s a = runFn2 errorImpl s a

foreign import endImpl """
function endImpl(stream){
  return function EndEff(){
    stream.end();
  }
}""" :: forall a. a

end :: Stream (Emit _) (End _) _ _ -> EffKefir _ Unit
end = endImpl

foreign import emitAsyncImpl """
function emitAsyncImpl(stream, value){
  return function EmitAsyncEff(){
    setTimeout(function(){stream.emit(value);}, 0);
  }
}""" :: forall a. a

emitAsync :: forall a. Stream (Emit _) (Obs _) _ a -> a -> EffKefir _ Unit
emitAsync s a = runFn2 emitAsyncImpl s a

foreign import errorAsyncImpl """
function errorAsyncImpl(stream, value){
  return function ErrorAsyncEff(){
    setTimeout(function(){stream.error(value);}, 0);
  }
}""" :: forall a. a

errorAsync :: forall e. Stream (Emit _) (Err _) e _ -> e -> EffKefir _ Unit
errorAsync s a = runFn2 errorAsyncImpl s a

foreign import endAsyncImpl """
function endAsyncImpl(stream){
  return function EndAsyncEff(){
    setTimeout(function(){stream.end();}, 0);
  }
}""" :: forall a. a

endAsync :: Stream (Emit _) (End _) _ _ -> EffKefir _ Unit
endAsync = endAsyncImpl

-- never
foreign import neverImpl """
function neverImpl(kefir){
  return function NeverEff(){
    return kefir.never();
  }
}""" :: forall a. a

never :: EffKefir _ (Stream () (End()) _ _)
never = neverImpl kefir

-- later
foreign import laterImpl """
function laterImpl(kefir, time, value){
  return function LaterEff(){
    return kefir['later'](time, value);
  }
}""" :: forall a. a

later :: forall a. Number -> a -> EffKefir _ (Stream () (ObsEnd()) _ a)
later t a = runFn3 laterImpl kefir t a

-- Interval
foreign import intervalImpl """
function intervalImpl(kefir, time, value){
  return function IntervalEff(){
    return kefir.interval(time, value);
  }
}""" :: forall a. a

interval :: forall a. Number -> a -> EffKefir _ (Stream () (Obs()) _ a)
interval t a = runFn3 intervalImpl kefir t a

-- sequentially
foreign import sequentiallyImpl """
function sequentiallyImpl(kefir, time, value){
  return function SequentiallyEff(){
    return kefir['sequentially'](time, value);
  }
}""" :: forall a. a

sequentially :: forall a. Number -> [a] -> EffKefir _ (Stream () (ObsEnd()) _ a)
sequentially t a = runFn3 sequentiallyImpl kefir t a

-- repeatedly
foreign import repeatedlyImpl """
function repeatedlyImpl(kefir, time, value){
  return function RepeatedlyEff(){
    return kefir['repeatedly'](time, value);
  }
}""" :: forall a. a

repeatedly :: forall a. Number -> [a] -> EffKefir _ (Stream () (Obs()) _ a)
repeatedly t a = runFn3 repeatedlyImpl kefir t a

-- fromPoll
foreign import fromPollImpl """
function fromPollImpl(kefir, time, func){
  return function FromPollEff(){
    return kefir['fromPoll'](time, func);
  }
}""" :: forall a. a

fromPoll :: forall e a. Number -> EffKefir e a -> EffKefir e (Stream () (Obs()) _ a)
fromPoll t a = runFn3 fromPollImpl kefir t a

-- withinterval
foreign import withIntervalImpl """
function withIntervalImpl(kefir, interval, cb){
  return function WithIntervalEff(){
    return kefir['withInterval'](interval, function(e){cb(e)();});
  }
}""" :: forall a. a

withInterval :: forall e a. Number -> (Stream (Emit()) (All()) _ a -> EffKefir e Unit) -> EffKefir e (Stream () (ObsEnd()) _ a)
withInterval t a = runFn3 withIntervalImpl kefir t a

-- fromCallback
foreign import fromCallbackImpl """
function fromCallbackImpl(kefir, fn){
  return function FromCallbackEff(){
    return kefir.fromCallback(function(cb){
      cb(fn());
    });
  }
}""" :: forall a. a

fromCallback :: forall e a. (EffKefir e a) -> EffKefir e (Stream () (ObsEnd()) _ a)
fromCallback f = runFn2 fromCallbackImpl kefir f

-- fromCallback
foreign import fromNodeCallbackImpl """
function fromNodeCallbackImpl(either, kefir, fn){
  return function FromNodeCallbackEff(){
    return kefir.fromNodeCallback(function(cb){
      either(function(e){cb(e);})(function(v){cb(null, v);})(fn());
    });
  }
}""" :: forall a. a

fromNodeCallback :: forall eff e a. (EffKefir eff (Either e a)) -> EffKefir eff (Stream () (All()) e a)
fromNodeCallback f = runFn3 fromNodeCallbackImpl either kefir f

-- TODO: fromSubUnsub

-- fromBinder
foreign import fromBinderImpl """
function fromBinderImpl(kefir, binder){
  return function FromBinderEff(){
    return kefir.fromBinder(function(emitter){
      return binder(emitter)();
    });
  }
}""" :: forall a. a

fromBinder :: forall e a. (Stream (Emit()) (All()) _ a -> EffKefir e (EffKefir e Unit)) -> EffKefir e (Stream () (All()) _ a)
fromBinder f = runFn2 fromBinderImpl kefir f

-- Property
foreign import constantImpl """
function constantImpl(kefir, a){
  return function CinstantEff(){
    return kefir.constant(a);
  }
}""" :: forall a. a

constant :: forall a. a -> EffKefir _ (Property () (ObsEnd()) _ a)
constant a = runFn2 constantImpl kefir a

foreign import constantErrorImpl """
function constantErrorImpl(kefir, a){
  return function CinstantErrorEff(){
    return kefir.constantError(a);
  }
}""" :: forall a. a

constantError :: forall e. e -> EffKefir _ (Property () (ErrEnd()) e _)
constantError e = runFn2 constantErrorImpl kefir e

-- TODO: fromPromise

-- Observable Impl

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
}""" :: forall eff stream p s e a b. Fn2 (stream p s e a) (a -> EffKefir eff b) (EffKefir eff (Unregister eff))

type Unregister e = EffKefir e Unit

onValue :: forall e a. Stream _ (Obs _) _ a -> (a -> EffKefir e _) -> EffKefir e (Unregister e)
onValue s f = runFn2 onValueImpl s f

foreign import onErrorImpl """
function onErrorImpl(str, fn){
  return function onErrorImplEff(){
    function onErrorCallback(x){
      fn(x)();
    };
    str.onError(onErrorCallback);
    return function offErrorEff() {
      str.offError(onErrorCallback);
    }
  }
}""" :: forall eff stream p s e a b. Fn2 (stream p s e a) (e -> EffKefir eff b) (EffKefir eff (Unregister eff))

onError :: forall eff e. Stream _ (Err _) e _ -> (e -> EffKefir eff _) -> EffKefir eff (Unregister eff)
onError s f = runFn2 onErrorImpl s f

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
}""" :: forall eff stream p s e a b. Fn2 (stream p s e a) (EffKefir eff b) (EffKefir eff (Unregister eff))

onEnd :: forall e. Stream _ (End _) _ _ -> (EffKefir e _) -> EffKefir e (Unregister e)
onEnd s f = runFn2 onEndImpl s f

data Event e a
  = Value Boolean a
  | Error Boolean e
  | End

foreign import onAnyImpl """
function onAnyImpl(cnsts, str, fn){
  return function onAnyImplEff(){
    function onAnyCallback(ev){
      if(ev.type === 'value') {
        var v = cnsts.value(ev.current, ev.value);
      } else if (ev.type === 'error') {
        var v = cnsts.error(ev.current, ev.value);
      } else {
        var v = cnsts.end;
      }
      return fn(v)();
    }
    str.onAny(onAnyCallback);
    return function offAnyEff(){
      str.offAny(onAnyCallback);
    }
  }
}""" :: forall a. a

onAny :: forall eff e a. Stream _ _ e a -> (Event e a -> EffKefir eff _) -> EffKefir eff (Unregister eff)
onAny s f = runFn3 onAnyImpl {value: mkFn2 $ \a b -> Value a b, error: mkFn2 $ \a b -> Error a b, end: End} s f

foreign import onLogImpl """
function onLogImpl (sream) {
  return function OnLogEff(){
    stream.log();
  }
}""" :: forall a. a

onLog :: Stream _ _ _ _ -> EffKefir _ Unit
onLog = onLogImpl

foreign import onLogWithImpl """
function onLogWithImpl (sream, str) {
  return function OnLogWithEff(){
    stream.log(str);
  }
}""" :: forall a. a

onLogWith :: Stream _ _ _ _ -> String -> EffKefir _ Unit
onLogWith s n = runFn2 onLogWithImpl s n

foreign import offLogImpl """
function offLogImpl (sream) {
  return function OffLogEff(){
    stream.offLog();
  }
}""" :: forall a. a


offLog :: Stream _ _ _ _ -> EffKefir _ Unit
offLog s = offLogImpl s

foreign import toPropertyImpl """
function toPropertyImpl(stream){
  return function ToPropertyEff(){
    return stream['toProperty']();
  }
}""" :: forall a. a

toProperty :: forall s e a. Stream _ s e a -> EffKefir _ (Property () s e a)
toProperty = toPropertyImpl

foreign import toPropertyWithImpl """
function toPropertyWithImpl(stream, v){
  return function ToPropertyWithEff(){
    return stream['toProperty'](v);
  }
}""" :: forall a. a

toPropertyWith :: forall s e a. a -> Stream _ s e a -> EffKefir _ (Property () s e a)
toPropertyWith w s = runFn2 toPropertyWithImpl s w

foreign import changesImpl """
function changesImpl(stream){
  return function ChangesEff(){
    return stream['changes']();
  }
}""" :: forall a. a

changes :: forall s e a. Property _ s e a -> EffKefir _ (Stream () s e a)
changes = changesImpl

-- modify an observable

foreign import mapImpl """
function mapImpl(fn, stream){
  return function MapEff(){
    return stream.map(fn);
  }
}""" :: forall a. a

map :: forall s e a b. (a -> b) -> Stream _ (Obs s) e a -> EffKefir _ (Stream () (Obs s) e b)
map f s = runFn2 mapImpl f s

foreign import mapEffImpl """
function mapEffImpl(fn, stream){
  return function MapEffEff(){
    return stream.map(function(a){return fn(a)();});
  }
}""" :: forall a. a

mapEff :: forall eff s e a b. (a -> EffKefir eff b) -> Stream _ (Obs s) e a -> EffKefir eff (Stream () (Obs s) e b)
mapEff f s = runFn2 mapEffImpl f s

foreign import filterImpl """
function filterImpl(fn, stream){
  return function FilterEff(){
    return stream.filter(fn);
  }
}""" :: forall a. a

filter :: forall s e a. (a -> Boolean) -> Stream _ (Obs s) e a -> EffKefir _ (Stream () (Obs s) e a)
filter f s = runFn2 filterImpl f s

foreign import filterEffImpl """
function filterEffImpl(fn, stream){
  return function FilterEffEff(){
    return stream.filter(function(a){fn(a)();});
  }
}""" :: forall a. a

filterEff :: forall eff s e a. (a -> EffKefir eff Boolean) -> Stream _ (Obs s) e a -> EffKefir eff (Stream () (Obs s) e a)
filterEff f s = runFn2 filterEffImpl f s

foreign import takeImpl """
function takeImpl(n, stream){
  return function TakeEff(){
    return stream['take'](n);
  }
}""" :: forall a. a

take :: forall e a. Number -> Stream _ (Obs _) e a -> EffKefir _ (Stream () (All()) e a)
take n s = runFn2 takeImpl n s

foreign import takeWhileImpl """
function takeWhileImpl(f, stream){
  return function TakeWhileEff(){
    return stream['takeWhile'](f);
  }
}""" :: forall a. a

takeWhile :: forall e a. (a -> Boolean) -> Stream _ (Obs _) e a -> EffKefir _ (Stream () (All()) e a)
takeWhile f s = runFn2 takeWhileImpl f s

foreign import takeWhileEffImpl """
function takeWhileEffImpl(f, stream){
  return function TakeWhileEffEff(){
    return stream['takeWhile'](function(a){return f(a)();});
  }
}""" :: forall a. a

takeWhileEff :: forall eff e a. (a -> EffKefir eff Boolean) -> Stream _ (Obs _) e a -> EffKefir eff (Stream () (All()) e a)
takeWhileEff f s = runFn2 takeWhileEffImpl f s

foreign import skipImpl """
function skipImpl(n, stream){
  return function SkipEff(){
    return stream['skip'](n);
  }
}""" :: forall a. a

skip :: forall s e a. Number -> Stream _ (Obs s) e a -> EffKefir _ (Stream () (Obs s) e a)
skip n s = runFn2 skipImpl n s

foreign import skipWhileImpl """
function skipWhileImpl(f, stream){
  return function SkipWhileEff(){
    return stream['skipWhile'](f);
  }
}""" :: forall a. a

skipWhile :: forall s e a. (a -> Boolean) -> Stream _ (Obs s) e a -> EffKefir _ (Stream () (Obs s) e a)
skipWhile f s = runFn2 skipWhileImpl f s

foreign import skipWhileEffImpl """
function skipWhileEffImpl(f, stream){
  return function SkipWhileEffEff(){
    return stream['skipWhile'](function(a){return f(a)();});
  }
}""" :: forall a. a

skipWhileEff :: forall eff s e a. (a -> EffKefir eff Boolean) -> Stream _ (Obs s) e a -> EffKefir eff (Stream () (Obs s) e a)
skipWhileEff f s = runFn2 skipWhileEffImpl f s

foreign import skipDuplicatesWithImpl """
function skipDuplicatesWithImpl(f, stream){
  return function SkipDuplicatesWithEff(){
    return stream['skipDuplicates'](f);
  }
}""" :: forall a. a

skipDuplicatesWith :: forall s e a. (a -> a -> Boolean) -> Stream _ (Obs s) e a -> EffKefir _ (Stream () (Obs s) e a)
skipDuplicatesWith f s = runFn2 skipDuplicatesWithImpl (mkFn2 $ \a b -> f a b) s

skipDuplicates :: forall e s a. (Eq a) => Stream _ (Obs s) e a -> EffKefir _ (Stream () (Obs s) e a)
skipDuplicates = skipDuplicatesWith (==)

foreign import diff1Impl """
function diff1Impl(f, stream){
  return function Diff1Eff(){
    return stream['diff'](f);
  }
}""" :: forall a. a

diff1 :: forall s e a b. (a -> a -> b) -> Stream _ (Obs s) e a -> EffKefir _ (Stream () (Obs s) e b)
diff1 f s = runFn2 diff1Impl (mkFn2 $ \a b -> f a b) s

foreign import diffImpl """
function diffImpl(f, a, stream){
  return function DiffEff(){
    return stream['diff'](f, a);
  }
}""" :: forall a. a

diff :: forall s e a b. (a -> a -> b) -> a -> Stream _ (Obs s) e a -> EffKefir _ (Stream () (Obs s) e b)
diff f a s = runFn3 diffImpl (mkFn2 $ \a b -> f a b) a s

foreign import scan1Impl """
function scan1Impl(f, stream){
  return function Scan1Eff(){
    return stream['scan'](f);
  }
}""" :: forall a. a

scan1 :: forall s e a. (a -> a -> a) -> Stream _ (Obs s) e a -> EffKefir _ (Property () (Obs s) e a)
scan1 f s = runFn2 scan1Impl (mkFn2 $ \a b -> f a b) s

foreign import scanImpl """
function scanImpl(f, a, stream){
  return function ScanEff(){
    return stream['scan'](f, a);
  }
}""" :: forall a. a

scan :: forall s e a b. (b -> a -> b) -> b -> Stream _ (Obs s) e a -> EffKefir _ (Property () (Obs s) e b)
scan f a s = runFn3 scanImpl (mkFn2 $ \a b -> f a b) a s

foreign import reduce1Impl """
function reduce1Impl(f, stream){
  return function Reduce1Eff(){
    return stream.reduce(f);
  }
}""" :: forall a. a

reduce1 :: forall s e a. (a -> a -> a) -> Stream _ (ObsEnd s) e a -> EffKefir _ (Stream () (ObsEnd s) e a)
reduce1 f s = runFn2 reduce1Impl (mkFn2 $ \a b -> f a b) s

foreign import reduceImpl """
function reduceImpl(f, a, stream){
  return function ReduceEff(){
    return stream.reduce(f, a);
  }
}""" :: forall a. a

reduce :: forall s e a b. (b -> a -> b) -> b -> Stream _ (ObsEnd s) e a -> EffKefir _ (Stream () (ObsEnd s) e b)
reduce f a s = runFn3 reduceImpl (mkFn2 $ \a b -> f a b) a s

foreign import reduceEff1Impl """
function reduceEff1Impl(f, stream){
  return function ReduceEff1Eff(){
    return stream.reduce(function(a,b){return f(a)(b)();});
  }
}""" :: forall a. a

reduceEff1 :: forall eff s e a. (a -> a -> EffKefir eff a) -> Stream _ (ObsEnd s) e a -> EffKefir eff (Stream () (ObsEnd s) e a)
reduceEff1 f s = runFn2 reduceEff1Impl f s

foreign import reduceEffImpl """
function reduceEffImpl(f, a, stream){
  return function ReduceEffEff(){
    return stream.reduce(function(a,b){return f(a)(b)();}, a);
  }
}""" :: forall a. a

reduceEff :: forall eff s e a b. (b -> a -> EffKefir eff b) -> b -> Stream _ (ObsEnd s) e a -> EffKefir eff (Stream () (ObsEnd s) e b)
reduceEff f a s = runFn3 reduceEffImpl f a s

foreign import mapEndImpl """
function mapEndImpl(f, stream){
  return function MapEndEff(){
    return stream['mapEnd'](function(){return f();});
  }
}""" :: forall a. a

mapEnd :: forall eff s e a. EffKefir eff a -> Stream _ (End s) e a -> EffKefir eff (Stream () (All()) e a)
mapEnd f s = runFn2 mapEndImpl f s

foreign import skipEndImpl """
function skipEndImpl(stream){
  return function SkipEndEff(){
    return stream['skipEnd']();
  }
}""" :: forall a. a

skipEnd :: forall s e a. Stream _ (End s) e a -> EffKefir _ (Stream () s e a)
skipEnd = skipEndImpl

foreign import slidingWindowImpl """
function slidingWindowImpl(min, max, stream){
  return function SlidingWindowEff(){
    return stream['slidingWindow'](max, min);
  }
}""" :: forall a. a

type Min = Number
type Max = Number
slidingWindow :: forall s e a. Min -> Max -> Stream _ (Obs s) e a -> EffKefir _ (Stream () (Obs s) e [a])
slidingWindow min max s = runFn3 slidingWindowImpl min max s

foreign import bufferWhileImpl """
function bufferWhileImpl(f, stream){
  return function BufferWhileEff(){
    return stream['bufferWhile'](f);
  }
}""" :: forall a. a

bufferWhile :: forall s e a. (a -> Boolean) -> Stream _ (Obs s) e a -> EffKefir _ (Stream () (Obs s) e [a])
bufferWhile f s = runFn2 bufferWhileImpl f s

foreign import bufferWhileWithImpl """
function bufferWhileWithImpl(opts, f, stream){
  return function BufferWhileEff(){
    return stream['bufferWhile'](f, {'flushOnEnd': opts.flushOnEnd});
  }
}""" :: forall a. a

bufferWhileWith :: forall s e a. {flushOnEnd :: Boolean} -> (a -> Boolean) -> Stream _ (Obs s) e a -> EffKefir _ (Stream () (Obs s) e [a])
bufferWhileWith opts f s = runFn3 bufferWhileWithImpl opts f s

foreign import delayImpl """
function delayImpl(d, stream){
  return function DelayEff(){
    return stream['delay'](d);
  }
}""" :: forall a. a

delay :: forall s e a. Number -> Stream _ (Obs s) e a -> EffKefir _ (Stream () (Obs s) e a)
delay w s = runFn2 delayImpl w s

foreign import throttleImpl """
function throttleImpl(d, stream){
  return function ThrottleEff(){
    return stream['throttle'](d);
  }
}""" :: forall a. a

throttle :: forall s e a. Number -> Stream _ (Obs s) e a -> EffKefir _ (Stream () (Obs s) e a)
throttle d s = runFn2 throttleImpl d s

foreign import throttleWithImpl """
function throttleWithImpl(opts, d, stream){
  return function ThrottleWithEff(){
    var o = {'leading': opts.leading, 'trailing': opts.trailing};
    return stream['throttle'](d, o);
  }
}""" :: forall a. a

throttleWith :: forall s e a. {leading :: Boolean, trailing :: Boolean} -> Number -> Stream _ (Obs s) e a -> EffKefir _ (Stream () (Obs s) e a)
throttleWith opts d s = runFn3 throttleWithImpl opts d s

foreign import debounceImpl """
function debounceImpl(d, stream){
  return function DebounceEff(){
    return stream['debounce'](d);
  }
}""" :: forall a. a

debounce :: forall s e a. Number -> Stream _ (Obs s) e a -> EffKefir _ (Stream () (Obs s) e a)
debounce d s = runFn2 debounceImpl d s

foreign import debounceWithImpl """
function debounceWithImpl(opts, d, stream){
  return function DebounceWithEff(){
    return stream['debounce'](d, {'immediate': opts.immediate});
  }
}""" :: forall a. a

debounceWith :: forall s e a. {immediate :: Boolean} -> Number -> Stream _ (Obs s) e a -> EffKefir _ (Stream () (Obs s) e a)
debounceWith opts d s = runFn3 debounceWithImpl opts d s

foreign import flattenImpl """
function flattenImpl(stream){
  return function FlattenEff(){
    return stream['flatten']();
  }
}""" :: forall a. a

flatten :: forall s e a. Stream _ (Obs s) e [a] -> EffKefir _ (Stream () (Obs s) e a)
flatten = flattenImpl

foreign import flattenWithImpl """
function flattenWithImpl(f, stream){
  return function FlattenWithEff(){
    return stream['flatten'](f);
  }
}""" :: forall a. a

flattenWith :: forall s e a b. (a -> [b]) -> Stream _ (Obs s) e a -> EffKefir _ (Stream () (Obs s) e b)
flattenWith f s = runFn2 flattenWithImpl f s

foreign import withHandlerImpl """
function withHandlerImpl(cnsts, src, fun){
  return function withHandlerImplEff(){
    return src['withHandler'](function(emitter, ev){
      if (ev.type === 'value') {
        var v = cnsts.value(ev.current, ev.value);
      } else if (ev.type === 'error') {
        var v = cnsts.error(ev.current, ev.value);
      } else {
        var v = cnsts.end;
      }
      fun(emitter, v)();
    });
  }
}""" :: forall a. a

withHandler :: forall eff e e' a b. Stream _ _ e a -> (Stream (Emit()) (All()) e' b -> Event e a -> EffKefir eff _) -> EffKefir eff (Stream () (All()) e' b)
withHandler s f = runFn3 withHandlerImpl
  {value: mkFn2 $ \a b -> Value a b, error: mkFn2 $ \a b -> Error a b, end: End} s (mkFn2 $ \a b -> f a b)

foreign import valuesToErrorsImpl """
function valuesToErrorsImpl(cnsts, f, stream){
  return function ValuesToErrorsEff(){
    function valuesToErrorsCallback(a){
      var mb = f(a);
      if(cnsts.isNothing(mb)) {
        return { 'convert': false, 'error': undefined }
      } else {
        return { 'convert': true, 'error': cnsts.fromJust(mb) }
      }
    }

    return stream.valuesToErrors(valuesToErrorsCallback);
  }
}""" :: forall a. a

valuesToErrors :: forall s e a. (a -> Maybe e) -> Stream _ (Obs s) e a -> EffKefir _ (Stream () (All()) e a)
valuesToErrors f s = runFn3 valuesToErrorsImpl { isNothing: isNothing, fromJust: fromJust } f s

foreign import errorsToValuesImpl """
function errorsToValuesImpl(cnsts, f, stream){
  return function ErrorsToValuesEff(){
    function errorsToValuesCallback(a){
      var mb = f(a);
      if(cnsts.isNothing(mb)) {
        return { 'convert': false, 'value': undefined }
      } else {
        return { 'convert': true, 'value': cnsts.fromJust(mb) }
      }
    }

    return stream.errorsToValues(errorsToValuesCallback);
  }
}""" :: forall a. a

errorsToValues :: forall s e a. (e -> Maybe a) -> Stream _ (Err s) e a -> EffKefir _ (Stream () (All()) e a)
errorsToValues f s = runFn3 errorsToValuesImpl { isNothing: isNothing, fromJust: fromJust } f s

foreign import mapErrorsImpl """
function mapErrorsImpl(f, stream){
  return function MapErrorsEff(){
    return stream.mapErrors(f);
  }
}""" :: forall a. a

mapErrors :: forall s e e' a. (e -> e') -> Stream _ (Err s) e a -> EffKefir _ (Stream () (Err s) e' a)
mapErrors f s = runFn2 mapErrorsImpl f s

foreign import mapErrorsEffImpl """
function mapErrorsEffImpl(f, stream){
  return function MapErrorsEffEff(){
    return stream.mapErrors(function(a){return f(a)();});
  }
}""" :: forall a. a

mapErrorsEff :: forall eff s e e' a. (e -> EffKefir eff e') -> Stream _ (Err s) e a -> EffKefir eff (Stream () (Err s) e' a)
mapErrorsEff f s = runFn2 mapErrorsEffImpl f s

foreign import filterErrorsImpl """
function filterErrorsImpl(f, stream){
  return function FilterErrorsEff(){
    return stream.filterErrors(f);
  }
}""" :: forall a. a

filterErrors :: forall s e a. (e -> Boolean) -> Stream _ (Err s) e a -> EffKefir _ (Stream () (Err s) e a)
filterErrors f s = runFn2 filterErrorsImpl f s

foreign import filterErrorsEffImpl """
function filterErrorsEffImpl(f, stream){
  return function FilterErrorsEffEff(){
    return stream.filterErrors(function(a){f(a)();});
  }
}""" :: forall a. a

filterErrorsEff :: forall eff s e a. (e -> EffKefir eff Boolean) -> Stream _ (Err s) e a -> EffKefir eff (Stream () (Err s) e a)
filterErrorsEff f s = runFn2 filterErrorsEffImpl f s

foreign import skipErrorsImpl """
function skipErrorsImpl(stream){
  return function SkipErrorsEff(){
    return stream.skipErrors();
  }
}""" :: forall a. a

skipErrors :: forall s a. Stream _ (Err s) _ a -> EffKefir _ (Stream () s _ a)
skipErrors = skipErrorsImpl

foreign import skipValuesImpl """
function skipValuesImpl(stream){
  return function SkipValuesEff(){
    return stream.skipValues();
  }
}""" :: forall a. a

skipValues :: forall s e. Stream _ (Obs s) e _ -> EffKefir _ (Stream () s e _)
skipValues = skipValuesImpl

foreign import endOnErrorImpl """
function endOnErrorImpl(stream){
  return function EndOnErrorEff(){
    return stream.endOnError();
  }
}""" :: forall a. a

endOnError :: forall s e. Stream _ (Err s) e _ -> EffKefir _ (Stream () (Err s) e _)
endOnError = endOnErrorImpl

-- Combine
-- TODO: combine multi stream
foreign import combineImpl """
function combineImpl(a, b, f) {
  return function CombineEff(){
    return a.combine(b, f);
  }
}""" :: forall a. a

combine :: forall e a b x. Stream _ _ e a -> Stream _ _ e b -> (a -> b -> x) -> EffKefir _ (Stream () (All()) e x)
combine a b f = runFn3 combineImpl a b (mkFn2 $ \a b -> f a b)

foreign import andImpl """
function andImpl(kefir, os) {
  return function AndEff(){
    return kefir.and(os);
  }
}""" :: forall a. a

and :: forall s e. [Stream _ s e Boolean] -> EffKefir _ (Stream () s e Boolean)
and os = runFn2 andImpl kefir os

foreign import orImpl """
function orImpl(kefir, os) {
  return function OrEff(){
    return kefir.or(os);
  }
}""" :: forall a. a

or :: forall s e. [Stream _ s e Boolean] -> EffKefir _ (Stream () s e Boolean)
or os = runFn2 orImpl kefir os

-- TODO: sampledBy multi stream
foreign import sampledByImpl """
function sampledByImpl(pas, act, fn) {
  return function SampledByEff(){
    return pas.sampledBy(act, fn);
  }
}""" :: forall a. a

type Passive = Stream
type Active  = Stream
sampledBy :: forall s e a b x. Passive _ _ e a -> Active _ _ e b -> (a -> b -> x) -> EffKefir _ (Stream () (All()) e x)
sampledBy pass act f = runFn3 sampledByImpl pass act (mkFn2 $ \a b -> f a b)

-- TODO: zip multi stream
foreign import zipWithImpl """
function zipWithImpl(f, a, b) {
  return function ZipWithEff(){
    return a.zip(b, f);
  }
}""" :: forall a. a

zipWith :: forall s e a b x. (a -> b -> x) -> Stream _ s e a -> Stream _ s e b -> EffKefir _ (Stream () s e x)
zipWith f a b = runFn3 zipWithImpl (mkFn2 $ \a b -> f a b) a b

foreign import mergeImpl """
function mergeImpl(kefir, os) {
  return function MergeEff(){
    return kefir.merge(os);
  }
}""" :: forall a. a

merge :: forall s e a. [Stream _ s e a] -> EffKefir _ (Stream () s e a)
merge os = runFn2 mergeImpl kefir os

foreign import concatImpl """
function concatImpl(kefir, os) {
  return function ConcatEff(){
    return kefir['concat'](os);
  }
}""" :: forall a. a

concat :: forall s e a. [Stream _ s e a] -> EffKefir _ (Stream () s e a)
concat os = runFn2 concatImpl kefir os

foreign import poolImpl """
function poolImpl(kefir) {
  return function PoolEff(){
    return kefir.pool();
  }
}""" :: forall a. a

pool :: forall eff e a. EffKefir eff (Stream (Plug()) (ObsErr()) e a)
pool = poolImpl kefir

foreign import plugImpl """
function plugImpl(pool, stream){
  return function PlugEff(){
    pool.plug(stream);
  }
}""" :: forall a. a

plug :: forall e a. Stream (Plug _) _ e a -> Stream _ _ e a -> EffKefir _ Unit
plug p s = runFn2 plugImpl p s

foreign import unPlugImpl """
function unPlugImpl(pool, stream){
  return function UnPlugEff(){
    pool.unplug(stream);
  }
}""" :: forall a. a

unPlug :: forall e a. Stream (Plug _) _ e a -> Stream _ _ e a -> EffKefir _ Unit
unPlug p s = runFn2 unPlugImpl p s

foreign import busImpl """
function busImpl(kefir) {
  return function BusEff(){
    return kefir.bus();
  }
}""" :: forall a. a

bus :: forall eff e a. EffKefir eff (Stream (EmitPlug()) (All()) e a)
bus = busImpl kefir

foreign import flatMapImpl """
function flatMapImpl(stream) {
  return function FlatMapEff(){
    return stream.flatMap();
  }
}""" :: forall a. a

flatMap :: forall e a. Stream _ _ e (Stream _ _ e a) -> EffKefir _ (Stream () (All()) e a)
flatMap = flatMapImpl

foreign import flatMapLatestImpl """
function flatMapLatestImpl(stream) {
  return function FlatMapLatestEff(){
    return stream.flatMapLatest();
  }
}""" :: forall a. a

flatMapLatest :: forall e a. Stream _ _ e (Stream _ _ e a) -> EffKefir _ (Stream () (All()) e a)
flatMapLatest = flatMapLatestImpl

foreign import flatMapFirstImpl """
function flatMapFirstImpl(stream) {
  return function FlatMapFirstEff(){
    return stream.flatMapFirst();
  }
}""" :: forall a. a

flatMapFirst :: forall e a. Stream _ _ e (Stream _ _ e a) -> EffKefir _ (Stream () (All()) e a)
flatMapFirst = flatMapFirstImpl

foreign import flatMapConcatImpl """
function flatMapConcatImpl(stream) {
  return function FlatMapConcatEff(){
    return stream.flatMapConcat();
  }
}""" :: forall a. a

flatMapConcat :: forall e a. Stream _ _ e (Stream _ _ e a) -> EffKefir _ (Stream () (All()) e a)
flatMapConcat = flatMapConcatImpl

foreign import flatMapWithImpl """
function flatMapWithImpl(stream, fn) {
  return function FlatMapWithEff(){
    return stream.flatMap(function(a){return fn(a)()});
  }
}""" :: forall a. a

flatMapWith :: forall eff e a b. Stream _ _ e a -> (a -> EffKefir eff (Stream _ _ e b)) -> EffKefir eff (Stream () (All()) e b)
flatMapWith s f = runFn2 flatMapWithImpl s f

foreign import flatMapLatestWithImpl """
function flatMapLatestWithImpl(stream, fn) {
  return function FlatMapLatestWithEff(){
    return stream.flatMapLatest(function(a){return fn(a)()});
  }
}""" :: forall a. a

flatMapLatestWith :: forall eff e a b. Stream _ _ e a -> (a -> EffKefir eff (Stream _ _ e b)) -> EffKefir eff (Stream () (All()) e b)
flatMapLatestWith s f = runFn2 flatMapLatestWithImpl s f

foreign import flatMapFirstWithImpl """
function flatMapFirstWithImpl(stream, fn) {
  return function FlatMapFirstWithEff(){
    return stream.flatMapFirst(function(a){return fn(a)()});
  }
}""" :: forall a. a

flatMapFirstWith :: forall eff e a b. Stream _ _ e a -> (a -> EffKefir eff (Stream _ _ e b)) -> EffKefir eff (Stream () (All()) e b)
flatMapFirstWith s f = runFn2 flatMapFirstWithImpl s f

foreign import flatMapConcatWithImpl """
function flatMapConcatWithImpl(stream, fn) {
  return function FlatMapConcatWithEff(){
    return stream.flatMapConcat(function(a){return fn(a)()});
  }
}""" :: forall a. a

flatMapConcatWith :: forall eff e a b. Stream _ _ e a -> (a -> EffKefir eff (Stream _ _ e b)) -> EffKefir eff (Stream () (All()) e b)
flatMapConcatWith s f = runFn2 flatMapConcatWithImpl s f

foreign import flatMapConcurLimitImpl """
function flatMapConcurLimitImpl(lim, stream) {
  return function FlatMapConcurLimitEff(){
    return stream.flatMapConcurLimit(undefined, lim);
  }
}""" :: forall a. a

flatMapConcurLimit :: forall e a. Number -> Stream _ _ e (Stream _ _ e a) -> EffKefir _ (Stream () (All()) e a)
flatMapConcurLimit l s = runFn2 flatMapConcurLimitImpl l s

foreign import flatMapConcurLimitWithImpl """
function flatMapConcurLimitWithImpl(lim, stream, fn) {
  return function FlatMapConcurLimitWithEff(){
    return stream.flatMapConcurLimit(function(a){return fn(a)()}, lim);
  }
}""" :: forall a. a

flatMapConcurLimitWith :: forall eff e a b. Number -> Stream _ _ e a -> (a -> EffKefir eff (Stream _ _ e b)) -> EffKefir eff (Stream () (All()) e b)
flatMapConcurLimitWith l s f = runFn3 flatMapConcurLimitWithImpl l s f

-- combine two observables
foreign import filterByImpl """
function filterByImpl(s, f){
  return function FilterEff(){
    return s['filterBy'](f);
  }
}""" :: forall a. a

filterBy :: forall e s a. Stream _ s e a -> Stream _ s e Boolean -> EffKefir _ (Stream () s e a)
filterBy s f = runFn2 filterByImpl s f

foreign import takeWhileByImpl """
function takeWhileByImpl(s, f){
  return function TakeWhileByEff(){
    return s['takeWhileBy'](f);
  }
}""" :: forall a. a

takeWhileBy :: forall e a. Stream _ _ e a -> Stream _ _ e Boolean -> EffKefir _ (Stream () (All()) e a)
takeWhileBy s f = runFn2 takeWhileByImpl s f

foreign import skipWhileByImpl """
function skipWhileByImpl(s, f){
  return function SkipWhileByEff(){
    return s['skipWhileBy'](f);
  }
}""" :: forall a. a

skipWhileBy :: forall s e a. Stream _ s e a -> Stream _ s e Boolean -> EffKefir _ (Stream () s e a)
skipWhileBy s f = runFn2 skipWhileByImpl s f

foreign import skipUntilByImpl """
function skipUntilByImpl(s, f){
  return function SkipUntilByEff(){
    return s['skipUntilBy'](f);
  }
}""" :: forall a. a

skipUntilBy :: forall s e a. Stream _ s e a -> Stream _ s e _ -> EffKefir _ (Stream () s e a)
skipUntilBy s f = runFn2 skipUntilByImpl s f

foreign import takeUntilByImpl """
function takeUntilByImpl(s, f){
  return function TakeUntilByEff(){
    return s['takeUntilBy'](f);
  }
}""" :: forall a. a

takeUntilBy :: forall e a. Stream _ _ e a -> Stream _ _ e _ -> EffKefir _ (Stream () (All()) e a)
takeUntilBy s f = runFn2 takeUntilByImpl s f

foreign import bufferByImpl """
function bufferByImpl(s, f){
  return function BufferByImpl(){
    return s['bufferBy'](f);
  }
}""" :: forall a. a

bufferBy :: forall s e a. Stream _ s e a -> Stream _ s e _ -> EffKefir _ (Stream () s e [a])
bufferBy s f = runFn2 bufferByImpl s f

foreign import bufferWhileByImpl """
function bufferWhileByImpl(s, f){
  return function BufferWhileByImpl(){
    return s['bufferWhileBy'](f);
  }
}""" :: forall a. a

bufferWhileBy :: forall s e a. Stream _ s e a -> Stream _ s e Boolean -> EffKefir _ (Stream () s e [a])
bufferWhileBy s f = runFn2 bufferWhileByImpl s f

foreign import awaitingImpl """
function awaitingImpl(s, f){
  return function AwaitingEff(){
    return s.awaiting(f);
  }
}""" :: forall a. a

type On = Stream
type Off = Stream
awaiting :: forall e s. On _ s e _ -> Off _ s e _ -> EffKefir _ (Property () s e Boolean)
awaiting s f = runFn2 awaitingImpl s f
