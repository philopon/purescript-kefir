# Module Documentation

## Module FRP.Kefir

#### `Kefir`

``` purescript
data Kefir :: !
```


#### `Stream`

``` purescript
data Stream :: # * -> # * -> * -> * -> *
```


#### `Property`

``` purescript
type Property = Stream
```


#### `EffKefir`

``` purescript
type EffKefir e = Eff (kefir :: Kefir | e)
```


#### `Terminable`

``` purescript
data Terminable :: *
```


#### `Observable`

``` purescript
data Observable :: *
```


#### `HasError`

``` purescript
data HasError :: *
```


#### `Emittable`

``` purescript
data Emittable :: *
```


#### `Pluggable`

``` purescript
data Pluggable :: *
```


#### `Obs`

``` purescript
type Obs s = (observable :: Observable | s)
```


#### `End`

``` purescript
type End s = (terminable :: Terminable | s)
```


#### `Err`

``` purescript
type Err s = (hasError :: HasError | s)
```


#### `ObsEnd`

``` purescript
type ObsEnd s = (terminable :: Terminable, observable :: Observable | s)
```


#### `ObsErr`

``` purescript
type ObsErr s = (hasError :: HasError, observable :: Observable | s)
```


#### `ErrEnd`

``` purescript
type ErrEnd s = (terminable :: Terminable, hasError :: HasError | s)
```


#### `All`

``` purescript
type All s = (terminable :: Terminable, observable :: Observable, hasError :: HasError | s)
```


#### `Emit`

``` purescript
type Emit s = (emittable :: Emittable | s)
```


#### `Plug`

``` purescript
type Plug s = (pluggable :: Pluggable | s)
```


#### `EmitPlug`

``` purescript
type EmitPlug s = (pluggable :: Pluggable, emittable :: Emittable | s)
```


#### `forget`

``` purescript
forget :: forall e a. Stream _ _ e a -> Stream _ _ e a
```


#### `unsafeGlobalize`

``` purescript
unsafeGlobalize :: forall p s e a. EffKefir _ (Stream p s e a) -> Stream p s e a
```


#### `emitter`

``` purescript
emitter :: EffKefir _ (Stream (Emit ()) (All ()) _ _)
```


#### `emit`

``` purescript
emit :: forall a. Stream (Emit _) (Obs _) _ a -> a -> EffKefir _ Unit
```


#### `error`

``` purescript
error :: forall e. Stream (Emit _) (Err _) e _ -> e -> EffKefir _ Unit
```


#### `end`

``` purescript
end :: Stream (Emit _) (End _) _ _ -> EffKefir _ Unit
```


#### `emitAsync`

``` purescript
emitAsync :: forall a. Stream (Emit _) (Obs _) _ a -> a -> EffKefir _ Unit
```


#### `errorAsync`

``` purescript
errorAsync :: forall e. Stream (Emit _) (Err _) e _ -> e -> EffKefir _ Unit
```


#### `endAsync`

``` purescript
endAsync :: Stream (Emit _) (End _) _ _ -> EffKefir _ Unit
```


#### `never`

``` purescript
never :: EffKefir _ (Stream () (End ()) _ _)
```


#### `later`

``` purescript
later :: forall a. Number -> a -> EffKefir _ (Stream () (ObsEnd ()) _ a)
```


#### `interval`

``` purescript
interval :: forall a. Number -> a -> EffKefir _ (Stream () (Obs ()) _ a)
```


#### `sequentially`

``` purescript
sequentially :: forall a. Number -> [a] -> EffKefir _ (Stream () (ObsEnd ()) _ a)
```


#### `repeatedly`

``` purescript
repeatedly :: forall a. Number -> [a] -> EffKefir _ (Stream () (Obs ()) _ a)
```


#### `fromPoll`

``` purescript
fromPoll :: forall e a. Number -> EffKefir e a -> EffKefir e (Stream () (Obs ()) _ a)
```


#### `withInterval`

``` purescript
withInterval :: forall e a. Number -> (Stream (Emit ()) (All ()) _ a -> EffKefir e Unit) -> EffKefir e (Stream () (ObsEnd ()) _ a)
```


#### `fromCallback`

``` purescript
fromCallback :: forall e a. EffKefir e a -> EffKefir e (Stream () (ObsEnd ()) _ a)
```


#### `fromNodeCallback`

``` purescript
fromNodeCallback :: forall eff e a. EffKefir eff (Either e a) -> EffKefir eff (Stream () (All ()) e a)
```


#### `fromBinder`

``` purescript
fromBinder :: forall e a. (Stream (Emit ()) (All ()) _ a -> EffKefir e (EffKefir e Unit)) -> EffKefir e (Stream () (All ()) _ a)
```


#### `constant`

``` purescript
constant :: forall a. a -> EffKefir _ (Property () (ObsEnd ()) _ a)
```


#### `constantError`

``` purescript
constantError :: forall e. e -> EffKefir _ (Property () (ErrEnd ()) e _)
```


#### `Unregister`

``` purescript
type Unregister e = EffKefir e Unit
```


#### `onValue`

``` purescript
onValue :: forall e a. Stream _ (Obs _) _ a -> (a -> EffKefir e _) -> EffKefir e (Unregister e)
```


#### `onError`

``` purescript
onError :: forall eff e. Stream _ (Err _) e _ -> (e -> EffKefir eff _) -> EffKefir eff (Unregister eff)
```


#### `onEnd`

``` purescript
onEnd :: forall e. Stream _ (End _) _ _ -> EffKefir e _ -> EffKefir e (Unregister e)
```


#### `Event`

``` purescript
data Event e a
  = Value Boolean a
  | Error Boolean e
  | End 
```


#### `onAny`

``` purescript
onAny :: forall eff e a. Stream _ _ e a -> (Event e a -> EffKefir eff _) -> EffKefir eff (Unregister eff)
```


#### `onLog`

``` purescript
onLog :: Stream _ _ _ _ -> EffKefir _ Unit
```


#### `onLogWith`

``` purescript
onLogWith :: Stream _ _ _ _ -> String -> EffKefir _ Unit
```


#### `offLog`

``` purescript
offLog :: Stream _ _ _ _ -> EffKefir _ Unit
```


#### `toProperty`

``` purescript
toProperty :: forall s e a. Stream _ s e a -> EffKefir _ (Property () s e a)
```


#### `toPropertyWith`

``` purescript
toPropertyWith :: forall s e a. a -> Stream _ s e a -> EffKefir _ (Property () s e a)
```


#### `changes`

``` purescript
changes :: forall s e a. Property _ s e a -> EffKefir _ (Stream () s e a)
```


#### `map`

``` purescript
map :: forall s e a b. (a -> b) -> Stream _ (Obs s) e a -> EffKefir _ (Stream () (Obs s) e b)
```


#### `mapEff`

``` purescript
mapEff :: forall eff s e a b. (a -> EffKefir eff b) -> Stream _ (Obs s) e a -> EffKefir eff (Stream () (Obs s) e b)
```


#### `filter`

``` purescript
filter :: forall s e a. (a -> Boolean) -> Stream _ (Obs s) e a -> EffKefir _ (Stream () (Obs s) e a)
```


#### `filterEff`

``` purescript
filterEff :: forall eff s e a. (a -> EffKefir eff Boolean) -> Stream _ (Obs s) e a -> EffKefir eff (Stream () (Obs s) e a)
```


#### `take`

``` purescript
take :: forall e a. Number -> Stream _ (Obs _) e a -> EffKefir _ (Stream () (All ()) e a)
```


#### `takeWhile`

``` purescript
takeWhile :: forall e a. (a -> Boolean) -> Stream _ (Obs _) e a -> EffKefir _ (Stream () (All ()) e a)
```


#### `takeWhileEff`

``` purescript
takeWhileEff :: forall eff e a. (a -> EffKefir eff Boolean) -> Stream _ (Obs _) e a -> EffKefir eff (Stream () (All ()) e a)
```


#### `skip`

``` purescript
skip :: forall s e a. Number -> Stream _ (Obs s) e a -> EffKefir _ (Stream () (Obs s) e a)
```


#### `skipWhile`

``` purescript
skipWhile :: forall s e a. (a -> Boolean) -> Stream _ (Obs s) e a -> EffKefir _ (Stream () (Obs s) e a)
```


#### `skipWhileEff`

``` purescript
skipWhileEff :: forall eff s e a. (a -> EffKefir eff Boolean) -> Stream _ (Obs s) e a -> EffKefir eff (Stream () (Obs s) e a)
```


#### `skipDuplicatesWith`

``` purescript
skipDuplicatesWith :: forall s e a. (a -> a -> Boolean) -> Stream _ (Obs s) e a -> EffKefir _ (Stream () (Obs s) e a)
```


#### `skipDuplicates`

``` purescript
skipDuplicates :: forall e s a. (Eq a) => Stream _ (Obs s) e a -> EffKefir _ (Stream () (Obs s) e a)
```


#### `diff1`

``` purescript
diff1 :: forall s e a b. (a -> a -> b) -> Stream _ (Obs s) e a -> EffKefir _ (Stream () (Obs s) e b)
```


#### `diff`

``` purescript
diff :: forall s e a b. (a -> a -> b) -> a -> Stream _ (Obs s) e a -> EffKefir _ (Stream () (Obs s) e b)
```


#### `scan1`

``` purescript
scan1 :: forall s e a. (a -> a -> a) -> Stream _ (Obs s) e a -> EffKefir _ (Property () (Obs s) e a)
```


#### `scan`

``` purescript
scan :: forall s e a b. (b -> a -> b) -> b -> Stream _ (Obs s) e a -> EffKefir _ (Property () (Obs s) e b)
```


#### `reduce1`

``` purescript
reduce1 :: forall s e a. (a -> a -> a) -> Stream _ (ObsEnd s) e a -> EffKefir _ (Stream () (ObsEnd s) e a)
```


#### `reduce`

``` purescript
reduce :: forall s e a b. (b -> a -> b) -> b -> Stream _ (ObsEnd s) e a -> EffKefir _ (Stream () (ObsEnd s) e b)
```


#### `reduceEff1`

``` purescript
reduceEff1 :: forall eff s e a. (a -> a -> EffKefir eff a) -> Stream _ (ObsEnd s) e a -> EffKefir eff (Stream () (ObsEnd s) e a)
```


#### `reduceEff`

``` purescript
reduceEff :: forall eff s e a b. (b -> a -> EffKefir eff b) -> b -> Stream _ (ObsEnd s) e a -> EffKefir eff (Stream () (ObsEnd s) e b)
```


#### `mapEnd`

``` purescript
mapEnd :: forall eff s e a. EffKefir eff a -> Stream _ (End s) e a -> EffKefir eff (Stream () (All ()) e a)
```


#### `skipEnd`

``` purescript
skipEnd :: forall s e a. Stream _ (End s) e a -> EffKefir _ (Stream () s e a)
```


#### `Min`

``` purescript
type Min = Number
```


#### `Max`

``` purescript
type Max = Number
```


#### `slidingWindow`

``` purescript
slidingWindow :: forall s e a. Min -> Max -> Stream _ (Obs s) e a -> EffKefir _ (Stream () (Obs s) e [a])
```


#### `bufferWhile`

``` purescript
bufferWhile :: forall s e a. (a -> Boolean) -> Stream _ (Obs s) e a -> EffKefir _ (Stream () (Obs s) e [a])
```


#### `bufferWhileWith`

``` purescript
bufferWhileWith :: forall s e a. { flushOnEnd :: Boolean } -> (a -> Boolean) -> Stream _ (Obs s) e a -> EffKefir _ (Stream () (Obs s) e [a])
```


#### `delay`

``` purescript
delay :: forall s e a. Number -> Stream _ (Obs s) e a -> EffKefir _ (Stream () (Obs s) e a)
```


#### `throttle`

``` purescript
throttle :: forall s e a. Number -> Stream _ (Obs s) e a -> EffKefir _ (Stream () (Obs s) e a)
```


#### `throttleWith`

``` purescript
throttleWith :: forall s e a. { trailing :: Boolean, leading :: Boolean } -> Number -> Stream _ (Obs s) e a -> EffKefir _ (Stream () (Obs s) e a)
```


#### `debounce`

``` purescript
debounce :: forall s e a. Number -> Stream _ (Obs s) e a -> EffKefir _ (Stream () (Obs s) e a)
```


#### `debounceWith`

``` purescript
debounceWith :: forall s e a. { immediate :: Boolean } -> Number -> Stream _ (Obs s) e a -> EffKefir _ (Stream () (Obs s) e a)
```


#### `flatten`

``` purescript
flatten :: forall s e a. Stream _ (Obs s) e [a] -> EffKefir _ (Stream () (Obs s) e a)
```


#### `flattenWith`

``` purescript
flattenWith :: forall s e a b. (a -> [b]) -> Stream _ (Obs s) e a -> EffKefir _ (Stream () (Obs s) e b)
```


#### `withHandler`

``` purescript
withHandler :: forall eff e e' a b. Stream _ _ e a -> (Stream (Emit ()) (All ()) e' b -> Event e a -> EffKefir eff _) -> EffKefir eff (Stream () (All ()) e' b)
```


#### `valuesToErrors`

``` purescript
valuesToErrors :: forall s e a. (a -> Maybe e) -> Stream _ (Obs s) e a -> EffKefir _ (Stream () (All ()) e a)
```


#### `errorsToValues`

``` purescript
errorsToValues :: forall s e a. (e -> Maybe a) -> Stream _ (Err s) e a -> EffKefir _ (Stream () (All ()) e a)
```


#### `mapErrors`

``` purescript
mapErrors :: forall s e e' a. (e -> e') -> Stream _ (Err s) e a -> EffKefir _ (Stream () (Err s) e' a)
```


#### `mapErrorsEff`

``` purescript
mapErrorsEff :: forall eff s e e' a. (e -> EffKefir eff e') -> Stream _ (Err s) e a -> EffKefir eff (Stream () (Err s) e' a)
```


#### `filterErrors`

``` purescript
filterErrors :: forall s e a. (e -> Boolean) -> Stream _ (Err s) e a -> EffKefir _ (Stream () (Err s) e a)
```


#### `filterErrorsEff`

``` purescript
filterErrorsEff :: forall eff s e a. (e -> EffKefir eff Boolean) -> Stream _ (Err s) e a -> EffKefir eff (Stream () (Err s) e a)
```


#### `skipErrors`

``` purescript
skipErrors :: forall s a. Stream _ (Err s) _ a -> EffKefir _ (Stream () s _ a)
```


#### `skipValues`

``` purescript
skipValues :: forall s e. Stream _ (Obs s) e _ -> EffKefir _ (Stream () s e _)
```


#### `endOnError`

``` purescript
endOnError :: forall s e. Stream _ (Err s) e _ -> EffKefir _ (Stream () (Err s) e _)
```


#### `combine`

``` purescript
combine :: forall e a b x. Stream _ _ e a -> Stream _ _ e b -> (a -> b -> x) -> EffKefir _ (Stream () (All ()) e x)
```


#### `and`

``` purescript
and :: forall s e. [Stream _ s e Boolean] -> EffKefir _ (Stream () s e Boolean)
```


#### `or`

``` purescript
or :: forall s e. [Stream _ s e Boolean] -> EffKefir _ (Stream () s e Boolean)
```


#### `Passive`

``` purescript
type Passive = Stream
```


#### `Active`

``` purescript
type Active = Stream
```


#### `sampledBy`

``` purescript
sampledBy :: forall s e a b x. Passive _ _ e a -> Active _ _ e b -> (a -> b -> x) -> EffKefir _ (Stream () (All ()) e x)
```


#### `zipWith`

``` purescript
zipWith :: forall s e a b x. (a -> b -> x) -> Stream _ s e a -> Stream _ s e b -> EffKefir _ (Stream () s e x)
```


#### `merge`

``` purescript
merge :: forall s e a. [Stream _ s e a] -> EffKefir _ (Stream () s e a)
```


#### `concat`

``` purescript
concat :: forall s e a. [Stream _ s e a] -> EffKefir _ (Stream () s e a)
```


#### `pool`

``` purescript
pool :: forall eff e a. EffKefir eff (Stream (Plug ()) (ObsErr ()) e a)
```


#### `plug`

``` purescript
plug :: forall e a. Stream (Plug _) _ e a -> Stream _ _ e a -> EffKefir _ Unit
```


#### `unPlug`

``` purescript
unPlug :: forall e a. Stream (Plug _) _ e a -> Stream _ _ e a -> EffKefir _ Unit
```


#### `bus`

``` purescript
bus :: forall eff e a. EffKefir eff (Stream (EmitPlug ()) (All ()) e a)
```


#### `flatMap`

``` purescript
flatMap :: forall e a. Stream _ _ e (Stream _ _ e a) -> EffKefir _ (Stream () (All ()) e a)
```


#### `flatMapLatest`

``` purescript
flatMapLatest :: forall e a. Stream _ _ e (Stream _ _ e a) -> EffKefir _ (Stream () (All ()) e a)
```


#### `flatMapFirst`

``` purescript
flatMapFirst :: forall e a. Stream _ _ e (Stream _ _ e a) -> EffKefir _ (Stream () (All ()) e a)
```


#### `flatMapConcat`

``` purescript
flatMapConcat :: forall e a. Stream _ _ e (Stream _ _ e a) -> EffKefir _ (Stream () (All ()) e a)
```


#### `flatMapWith`

``` purescript
flatMapWith :: forall eff e a b. Stream _ _ e a -> (a -> EffKefir eff (Stream _ _ e b)) -> EffKefir eff (Stream () (All ()) e b)
```


#### `flatMapLatestWith`

``` purescript
flatMapLatestWith :: forall eff e a b. Stream _ _ e a -> (a -> EffKefir eff (Stream _ _ e b)) -> EffKefir eff (Stream () (All ()) e b)
```


#### `flatMapFirstWith`

``` purescript
flatMapFirstWith :: forall eff e a b. Stream _ _ e a -> (a -> EffKefir eff (Stream _ _ e b)) -> EffKefir eff (Stream () (All ()) e b)
```


#### `flatMapConcatWith`

``` purescript
flatMapConcatWith :: forall eff e a b. Stream _ _ e a -> (a -> EffKefir eff (Stream _ _ e b)) -> EffKefir eff (Stream () (All ()) e b)
```


#### `flatMapConcurLimit`

``` purescript
flatMapConcurLimit :: forall e a. Number -> Stream _ _ e (Stream _ _ e a) -> EffKefir _ (Stream () (All ()) e a)
```


#### `flatMapConcurLimitWith`

``` purescript
flatMapConcurLimitWith :: forall eff e a b. Number -> Stream _ _ e a -> (a -> EffKefir eff (Stream _ _ e b)) -> EffKefir eff (Stream () (All ()) e b)
```


#### `filterBy`

``` purescript
filterBy :: forall e s a. Stream _ s e a -> Stream _ s e Boolean -> EffKefir _ (Stream () s e a)
```


#### `takeWhileBy`

``` purescript
takeWhileBy :: forall e a. Stream _ _ e a -> Stream _ _ e Boolean -> EffKefir _ (Stream () (All ()) e a)
```


#### `skipWhileBy`

``` purescript
skipWhileBy :: forall s e a. Stream _ s e a -> Stream _ s e Boolean -> EffKefir _ (Stream () s e a)
```


#### `skipUntilBy`

``` purescript
skipUntilBy :: forall s e a. Stream _ s e a -> Stream _ s e _ -> EffKefir _ (Stream () s e a)
```


#### `takeUntilBy`

``` purescript
takeUntilBy :: forall e a. Stream _ _ e a -> Stream _ _ e _ -> EffKefir _ (Stream () (All ()) e a)
```


#### `bufferBy`

``` purescript
bufferBy :: forall s e a. Stream _ s e a -> Stream _ s e _ -> EffKefir _ (Stream () s e [a])
```


#### `bufferWhileBy`

``` purescript
bufferWhileBy :: forall s e a. Stream _ s e a -> Stream _ s e Boolean -> EffKefir _ (Stream () s e [a])
```


#### `On`

``` purescript
type On = Stream
```


#### `Off`

``` purescript
type Off = Stream
```


#### `awaiting`

``` purescript
awaiting :: forall e s. On _ s e _ -> Off _ s e _ -> EffKefir _ (Property () s e Boolean)
```




