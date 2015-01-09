# Module Documentation

## Module FRP.Kefir

### Types

    type Active = Stream

    type All s = (terminable :: Terminable, observable :: Observable, hasError :: HasError | s)

    type EffKefir e = Eff (kefir :: Kefir | e)

    type Emit s = (emittable :: Emittable | s)

    type EmitPlug s = (pluggable :: Pluggable, emittable :: Emittable | s)

    data Emittable :: *

    type End s = (terminable :: Terminable | s)

    type Err s = (hasError :: HasError | s)

    type ErrEnd s = (terminable :: Terminable, hasError :: HasError | s)

    data Event e a where
      Value :: Boolean -> a -> Event e a
      Error :: Boolean -> e -> Event e a
      End :: Event e a

    data HasError :: *

    data Kefir :: !

    type Max = Number

    type Min = Number

    type Obs s = (observable :: Observable | s)

    type ObsEnd s = (terminable :: Terminable, observable :: Observable | s)

    type ObsErr s = (hasError :: HasError, observable :: Observable | s)

    data Observable :: *

    type Off = Stream

    type On = Stream

    type Passive = Stream

    type Plug s = (pluggable :: Pluggable | s)

    data Pluggable :: *

    type Property = Stream

    data Stream :: # * -> # * -> * -> * -> *

    data Terminable :: *

    type Unregister e = EffKefir e Unit


### Values

    and :: forall s e. [Stream _ s e Boolean] -> EffKefir _ (Stream () s e Boolean)

    awaiting :: forall e s. On _ s e _ -> Off _ s e _ -> EffKefir _ (Property () s e Boolean)

    bufferBy :: forall s e a. Stream _ s e a -> Stream _ s e _ -> EffKefir _ (Stream () s e [a])

    bufferWhile :: forall s e a. (a -> Boolean) -> Stream _ (Obs s) e a -> EffKefir _ (Stream () (Obs s) e [a])

    bufferWhileBy :: forall s e a. Stream _ s e a -> Stream _ s e Boolean -> EffKefir _ (Stream () s e [a])

    bufferWhileWith :: forall s e a. { flushOnEnd :: Boolean } -> (a -> Boolean) -> Stream _ (Obs s) e a -> EffKefir _ (Stream () (Obs s) e [a])

    bus :: forall eff e a. EffKefir eff (Stream (EmitPlug ()) (All ()) e a)

    changes :: forall s e a. Property _ s e a -> EffKefir _ (Stream () s e a)

    combine :: forall e a b x. Stream _ _ e a -> Stream _ _ e b -> (a -> b -> x) -> EffKefir _ (Stream () (All ()) e x)

    concat :: forall s e a. [Stream _ s e a] -> EffKefir _ (Stream () s e a)

    constant :: forall a. a -> EffKefir _ (Property () (ObsEnd ()) _ a)

    constantError :: forall e. e -> EffKefir _ (Property () (ErrEnd ()) e _)

    debounce :: forall s e a. Number -> Stream _ (Obs s) e a -> EffKefir _ (Stream () (Obs s) e a)

    debounceWith :: forall s e a. { immediate :: Boolean } -> Number -> Stream _ (Obs s) e a -> EffKefir _ (Stream () (Obs s) e a)

    delay :: forall s e a. Number -> Stream _ (Obs s) e a -> EffKefir _ (Stream () (Obs s) e a)

    diff :: forall s e a b. (a -> a -> b) -> a -> Stream _ (Obs s) e a -> EffKefir _ (Stream () (Obs s) e b)

    diff1 :: forall s e a b. (a -> a -> b) -> Stream _ (Obs s) e a -> EffKefir _ (Stream () (Obs s) e b)

    emit :: forall a. Stream (Emit _) (Obs _) _ a -> a -> EffKefir _ Unit

    emitAsync :: forall a. Stream (Emit _) (Obs _) _ a -> a -> EffKefir _ Unit

    emitter :: EffKefir _ (Stream (Emit ()) (All ()) _ _)

    end :: Stream (Emit _) (End _) _ _ -> EffKefir _ Unit

    endAsync :: Stream (Emit _) (End _) _ _ -> EffKefir _ Unit

    endOnError :: forall s e. Stream _ (Err s) e _ -> EffKefir _ (Stream () (ObsEnd ()) e _)

    error :: forall e. Stream (Emit _) (Err _) e _ -> e -> EffKefir _ Unit

    errorAsync :: forall e. Stream (Emit _) (Err _) e _ -> e -> EffKefir _ Unit

    errorsToValues :: forall s e a. (e -> Maybe a) -> Stream _ (Err s) e a -> EffKefir _ (Stream () (All ()) e a)

    filter :: forall s e a. (a -> Boolean) -> Stream _ (Obs s) e a -> EffKefir _ (Stream () (Obs s) e a)

    filterBy :: forall e s a. Stream _ s e a -> Stream _ s e Boolean -> EffKefir _ (Stream () s e a)

    filterEff :: forall eff s e a. (a -> EffKefir eff Boolean) -> Stream _ (Obs s) e a -> EffKefir eff (Stream () (Obs s) e a)

    filterErrors :: forall s e a. (e -> Boolean) -> Stream _ (Err s) e a -> EffKefir _ (Stream () (Err s) e a)

    filterErrorsEff :: forall eff s e a. (e -> EffKefir eff Boolean) -> Stream _ (Err s) e a -> EffKefir eff (Stream () (Err s) e a)

    flatMap :: forall e a. Stream _ _ e (Stream _ _ e a) -> EffKefir _ (Stream () (All ()) e a)

    flatMapConcat :: forall e a. Stream _ _ e (Stream _ _ e a) -> EffKefir _ (Stream () (All ()) e a)

    flatMapConcatWith :: forall eff e a b. Stream _ _ e a -> (a -> EffKefir eff (Stream _ _ e b)) -> EffKefir eff (Stream () (All ()) e b)

    flatMapConcurLimit :: forall e a. Number -> Stream _ _ e (Stream _ _ e a) -> EffKefir _ (Stream () (All ()) e a)

    flatMapConcurLimitWith :: forall eff e a b. Number -> Stream _ _ e a -> (a -> EffKefir eff (Stream _ _ e b)) -> EffKefir eff (Stream () (All ()) e b)

    flatMapFirst :: forall e a. Stream _ _ e (Stream _ _ e a) -> EffKefir _ (Stream () (All ()) e a)

    flatMapFirstWith :: forall eff e a b. Stream _ _ e a -> (a -> EffKefir eff (Stream _ _ e b)) -> EffKefir eff (Stream () (All ()) e b)

    flatMapLatest :: forall e a. Stream _ _ e (Stream _ _ e a) -> EffKefir _ (Stream () (All ()) e a)

    flatMapLatestWith :: forall eff e a b. Stream _ _ e a -> (a -> EffKefir eff (Stream _ _ e b)) -> EffKefir eff (Stream () (All ()) e b)

    flatMapWith :: forall eff e a b. Stream _ _ e a -> (a -> EffKefir eff (Stream _ _ e b)) -> EffKefir eff (Stream () (All ()) e b)

    flatten :: forall s e a. Stream _ (Obs s) e [a] -> EffKefir _ (Stream () (Obs s) e a)

    flattenWith :: forall s e a b. (a -> [b]) -> Stream _ (Obs s) e a -> EffKefir _ (Stream () (Obs s) e b)

    forget :: forall e a. Stream _ _ e a -> Stream _ _ e a

    fromBinder :: forall e a. (Stream (Emit ()) (All ()) _ a -> EffKefir e (EffKefir e Unit)) -> EffKefir e (Stream () (All ()) _ a)

    fromCallback :: forall e a. ((a -> EffKefir e Unit) -> EffKefir e Unit) -> EffKefir e (Stream () (ObsEnd ()) _ a)

    fromNodeCallback :: forall eff e a. ((Either e a -> EffKefir eff Unit) -> EffKefir eff Unit) -> EffKefir eff (Stream () (All ()) e a)

    fromPoll :: forall e a. Number -> EffKefir e a -> EffKefir e (Stream () (Obs ()) _ a)

    interval :: forall a. Number -> a -> EffKefir _ (Stream () (Obs ()) _ a)

    later :: forall a. Number -> a -> EffKefir _ (Stream () (ObsEnd ()) _ a)

    map :: forall s e a b. (a -> b) -> Stream _ (Obs s) e a -> EffKefir _ (Stream () (Obs s) e b)

    mapEff :: forall eff s e a b. (a -> EffKefir eff b) -> Stream _ (Obs s) e a -> EffKefir eff (Stream () (Obs s) e b)

    mapEnd :: forall eff s e a. EffKefir eff a -> Stream _ (End s) e a -> EffKefir eff (Stream () (All ()) e a)

    mapErrors :: forall s e e' a. (e -> e') -> Stream _ (Err s) e a -> EffKefir _ (Stream () (Err s) e' a)

    mapErrorsEff :: forall eff s e e' a. (e -> EffKefir eff e') -> Stream _ (Err s) e a -> EffKefir eff (Stream () (Err s) e' a)

    merge :: forall s e a. [Stream _ s e a] -> EffKefir _ (Stream () s e a)

    never :: EffKefir _ (Stream () (End ()) _ _)

    offLog :: Stream _ _ _ _ -> EffKefir _ Unit

    onAny :: forall eff e a. Stream _ _ e a -> (Event e a -> EffKefir eff _) -> EffKefir eff (Unregister eff)

    onEnd :: forall e. Stream _ (End _) _ _ -> EffKefir e _ -> EffKefir e (Unregister e)

    onError :: forall eff e. Stream _ (Err _) e _ -> (e -> EffKefir eff _) -> EffKefir eff (Unregister eff)

    onLog :: Stream _ _ _ _ -> EffKefir _ Unit

    onLogWith :: Stream _ _ _ _ -> String -> EffKefir _ Unit

    onValue :: forall e a. Stream _ (Obs _) _ a -> (a -> EffKefir e _) -> EffKefir e (Unregister e)

    or :: forall s e. [Stream _ s e Boolean] -> EffKefir _ (Stream () s e Boolean)

    plug :: forall e a. Stream (Plug _) _ e a -> Stream _ _ e a -> EffKefir _ Unit

    pool :: forall eff e a. EffKefir eff (Stream (Plug ()) (ObsErr ()) e a)

    reduce :: forall s e a b. (b -> a -> b) -> b -> Stream _ (ObsEnd s) e a -> EffKefir _ (Stream () (ObsEnd s) e b)

    reduce1 :: forall s e a. (a -> a -> a) -> Stream _ (ObsEnd s) e a -> EffKefir _ (Stream () (ObsEnd s) e a)

    reduceEff :: forall eff s e a b. (b -> a -> EffKefir eff b) -> b -> Stream _ (ObsEnd s) e a -> EffKefir eff (Stream () (ObsEnd s) e b)

    reduceEff1 :: forall eff s e a. (a -> a -> EffKefir eff a) -> Stream _ (ObsEnd s) e a -> EffKefir eff (Stream () (ObsEnd s) e a)

    repeatedly :: forall a. Number -> [a] -> EffKefir _ (Stream () (Obs ()) _ a)

    sampledBy :: forall s e a b x. Passive _ _ e a -> Active _ _ e b -> (a -> b -> x) -> EffKefir _ (Stream () (All ()) e x)

    scan :: forall s e a b. (b -> a -> b) -> b -> Stream _ (Obs s) e a -> EffKefir _ (Property () (Obs s) e b)

    scan1 :: forall s e a. (a -> a -> a) -> Stream _ (Obs s) e a -> EffKefir _ (Property () (Obs s) e a)

    sequentially :: forall a. Number -> [a] -> EffKefir _ (Stream () (ObsEnd ()) _ a)

    skip :: forall s e a. Number -> Stream _ (Obs s) e a -> EffKefir _ (Stream () (Obs s) e a)

    skipDuplicates :: forall e s a. (Eq a) => Stream _ (Obs s) e a -> EffKefir _ (Stream () (Obs s) e a)

    skipDuplicatesWith :: forall s e a. (a -> a -> Boolean) -> Stream _ (Obs s) e a -> EffKefir _ (Stream () (Obs s) e a)

    skipEnd :: forall s e a. Stream _ (End s) e a -> EffKefir _ (Stream () s e a)

    skipErrors :: forall s a. Stream _ (Err s) _ a -> EffKefir _ (Stream () s _ a)

    skipUntilBy :: forall s e a. Stream _ s e a -> Stream _ s e _ -> EffKefir _ (Stream () s e a)

    skipValues :: forall s e. Stream _ (Obs s) e _ -> EffKefir _ (Stream () s e _)

    skipWhile :: forall s e a. (a -> Boolean) -> Stream _ (Obs s) e a -> EffKefir _ (Stream () (Obs s) e a)

    skipWhileBy :: forall s e a. Stream _ s e a -> Stream _ s e Boolean -> EffKefir _ (Stream () s e a)

    skipWhileEff :: forall eff s e a. (a -> EffKefir eff Boolean) -> Stream _ (Obs s) e a -> EffKefir eff (Stream () (Obs s) e a)

    slidingWindow :: forall s e a. Min -> Max -> Stream _ (Obs s) e a -> EffKefir _ (Stream () (Obs s) e [a])

    take :: forall e a. Number -> Stream _ (Obs _) e a -> EffKefir _ (Stream () (All ()) e a)

    takeUntilBy :: forall e a. Stream _ _ e a -> Stream _ _ e _ -> EffKefir _ (Stream () (All ()) e a)

    takeWhile :: forall e a. (a -> Boolean) -> Stream _ (Obs _) e a -> EffKefir _ (Stream () (All ()) e a)

    takeWhileBy :: forall e a. Stream _ _ e a -> Stream _ _ e Boolean -> EffKefir _ (Stream () (All ()) e a)

    takeWhileEff :: forall eff e a. (a -> EffKefir eff Boolean) -> Stream _ (Obs _) e a -> EffKefir eff (Stream () (All ()) e a)

    throttle :: forall s e a. Number -> Stream _ (Obs s) e a -> EffKefir _ (Stream () (Obs s) e a)

    throttleWith :: forall s e a. { trailing :: Boolean, leading :: Boolean } -> Number -> Stream _ (Obs s) e a -> EffKefir _ (Stream () (Obs s) e a)

    toProperty :: forall s e a. Stream _ s e a -> EffKefir _ (Property () s e a)

    toPropertyWith :: forall s e a. a -> Stream _ s e a -> EffKefir _ (Property () s e a)

    unPlug :: forall e a. Stream (Plug _) _ e a -> Stream _ _ e a -> EffKefir _ Unit

    unsafeGlobalize :: forall p s e a. EffKefir _ (Stream p s e a) -> Stream p s e a

    valuesToErrors :: forall s e a. (a -> Maybe e) -> Stream _ (Obs s) e a -> EffKefir _ (Stream () (All ()) e a)

    withHandler :: forall eff e e' a b. Stream _ _ e a -> (Stream (Emit ()) (All ()) e' b -> Event e a -> EffKefir eff _) -> EffKefir eff (Stream () (All ()) e' b)

    withInterval :: forall e a. Number -> (Stream (Emit ()) (All ()) _ a -> EffKefir e Unit) -> EffKefir e (Stream () (ObsEnd ()) _ a)

    zipWith :: forall s e a b x. (a -> b -> x) -> Stream _ s e a -> Stream _ s e b -> EffKefir _ (Stream () s e x)



