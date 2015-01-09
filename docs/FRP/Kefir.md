# Module Documentation

## Module FRP.Kefir

### Types

    type Active = Stream

    type E = HasE ()

    type EP = (pluggable :: Pluggable, emittable :: Emittable)

    type EffKefir e = Eff (kefir :: Kefir | e)

    data Emittable :: *

    data Event e a where
      Value :: Boolean -> a -> Event e a
      Error :: Boolean -> e -> Event e a
      End :: Event e a

    type HasE s = (emittable :: Emittable | s)

    type HasO s = (observable :: Observable | s)

    type HasP s = (pluggable :: Pluggable | s)

    type HasT s = (terminable :: Terminable | s)

    data Kefir :: !

    type Max = Number

    type Min = Number

    type O = HasO ()

    type OT = (terminable :: Terminable, observable :: Observable)

    data Observable :: *

    type Off = Stream

    type On = Stream

    type P = HasP ()

    type Passive = Stream

    data Pluggable :: *

    type Property = Stream

    data Stream :: # * -> # * -> * -> * -> *

    type T = HasT ()

    data Terminable :: *

    type Unregister e = EffKefir e Unit


### Values

    and :: forall s e. [Stream _ s e Boolean] -> EffKefir _ (Stream () s e Boolean)

    awaiting :: forall e s. On _ s e _ -> Off _ s e _ -> EffKefir _ (Property () s e Boolean)

    bufferBy :: forall s e a. Stream _ s e a -> Stream _ _ e _ -> EffKefir _ (Stream () s e [a])

    bufferWhile :: forall stream s e a. (a -> Boolean) -> stream _ s e a -> EffKefir _ (stream () s e [a])

    bufferWhileBy :: forall s e a. Stream _ s e a -> Stream _ _ e Boolean -> EffKefir _ (Stream () s e [a])

    bufferWhileWith :: forall stream s e a. { flushOnEnd :: Boolean } -> (a -> Boolean) -> stream _ s e a -> EffKefir _ (stream () s e [a])

    bus :: forall eff e a. EffKefir eff (Stream EP OT e a)

    changes :: forall s e a. Property _ s e a -> EffKefir _ (Stream () s e a)

    combine :: forall e a b x. Stream _ _ e a -> Stream _ _ e b -> (a -> b -> x) -> EffKefir _ (Stream () OT e x)

    concat :: forall s e a. [Stream _ s e a] -> EffKefir _ (Stream () s e a)

    constant :: forall a. a -> EffKefir _ (Property () OT _ a)

    constantError :: forall e. e -> EffKefir _ (Property () OT e _)

    debounce :: forall stream s e a. Number -> stream _ s e a -> EffKefir _ (stream () s e a)

    debounceWith :: forall stream s e a. { immediate :: Boolean } -> Number -> stream _ s e a -> EffKefir _ (stream () s e a)

    delay :: forall stream s e a. Number -> stream _ s e a -> EffKefir _ (stream () s e a)

    diff :: forall s e a b. (a -> a -> b) -> a -> Stream _ s e a -> EffKefir _ (Stream () s e b)

    diff1 :: forall s e a b. (a -> a -> b) -> Stream _ s e a -> EffKefir _ (Stream () s e b)

    emit :: forall a. Stream (HasE _) _ _ a -> a -> EffKefir _ Unit

    emitAsync :: forall a. Stream (HasE _) _ _ a -> a -> EffKefir _ Unit

    emitter :: EffKefir _ (Stream E OT _ _)

    end :: Stream (HasE _) _ _ _ -> EffKefir _ Unit

    endAsync :: Stream (HasE _) _ _ _ -> EffKefir _ Unit

    endOnError :: forall s e. Stream _ s e _ -> EffKefir _ (Stream () s e _)

    error :: forall e. Stream (HasE _) _ e _ -> e -> EffKefir _ Unit

    errorAsync :: forall e. Stream (HasE _) _ e _ -> e -> EffKefir _ Unit

    errorsToValues :: forall s e a. (e -> Maybe a) -> Stream _ s e a -> EffKefir _ (Stream () s e a)

    filter :: forall s e a. (a -> Boolean) -> Stream _ s e a -> EffKefir _ (Stream () s e a)

    filterBy :: forall e s a. Stream _ s e a -> Stream _ _ e Boolean -> EffKefir _ (Stream () s e a)

    filterEff :: forall eff s e a. (a -> EffKefir eff Boolean) -> Stream _ s e a -> EffKefir eff (Stream () s e a)

    filterErrors :: forall s e a. (e -> Boolean) -> Stream _ s e a -> EffKefir _ (Stream () s e a)

    filterErrorsEff :: forall eff s e a. (e -> EffKefir eff Boolean) -> Stream _ s e a -> EffKefir eff (Stream () s e a)

    flatMap :: forall e a. Stream _ _ e (Stream _ _ e a) -> EffKefir _ (Stream () OT e a)

    flatMapConcat :: forall e a. Stream _ _ e (Stream _ _ e a) -> EffKefir _ (Stream () OT e a)

    flatMapConcatWith :: forall eff e a b. Stream _ _ e a -> (a -> EffKefir eff (Stream _ _ e b)) -> EffKefir eff (Stream () OT e b)

    flatMapConcurLimit :: forall e a. Number -> Stream _ _ e (Stream _ _ e a) -> EffKefir _ (Stream () OT e a)

    flatMapConcurLimitWith :: forall eff e a b. Number -> Stream _ _ e a -> (a -> EffKefir eff (Stream _ _ e b)) -> EffKefir eff (Stream () OT e b)

    flatMapFirst :: forall e a. Stream _ _ e (Stream _ _ e a) -> EffKefir _ (Stream () OT e a)

    flatMapFirstWith :: forall eff e a b. Stream _ _ e a -> (a -> EffKefir eff (Stream _ _ e b)) -> EffKefir eff (Stream () OT e b)

    flatMapLatest :: forall e a. Stream _ _ e (Stream _ _ e a) -> EffKefir _ (Stream () OT e a)

    flatMapLatestWith :: forall eff e a b. Stream _ _ e a -> (a -> EffKefir eff (Stream _ _ e b)) -> EffKefir eff (Stream () OT e b)

    flatMapWith :: forall eff e a b. Stream _ _ e a -> (a -> EffKefir eff (Stream _ _ e b)) -> EffKefir eff (Stream () OT e b)

    flatten :: forall stream s e a. stream _ s e [a] -> EffKefir _ (stream () s e a)

    flattenWith :: forall stream s e a b. (a -> [b]) -> stream _ s e a -> EffKefir _ (stream () s e b)

    forget :: forall e a. Stream _ _ e a -> Stream _ _ e a

    fromBinder :: forall e a. (Stream E () _ a -> EffKefir e (EffKefir e Unit)) -> EffKefir e (Stream () OT _ a)

    fromCallback :: forall e a. ((a -> EffKefir e Unit) -> EffKefir e Unit) -> EffKefir e (Stream () OT _ a)

    fromNodeCallback :: forall eff e a. ((Either e a -> EffKefir eff Unit) -> EffKefir eff Unit) -> EffKefir eff (Stream () OT e a)

    fromPoll :: forall e a. Number -> EffKefir e a -> EffKefir e (Stream () O _ a)

    interval :: forall a. Number -> a -> EffKefir _ (Stream () O _ a)

    later :: forall a. Number -> a -> EffKefir _ (Stream () OT _ a)

    map :: forall s e a b. (a -> b) -> Stream _ s e a -> EffKefir _ (Stream () s e b)

    mapEff :: forall eff s e a b. (a -> EffKefir eff b) -> Stream _ s e a -> EffKefir eff (Stream () s e b)

    mapEnd :: forall eff stream s e a. EffKefir eff a -> stream _ (HasT s) e a -> EffKefir eff (stream () OT e a)

    mapErrors :: forall s e e' a. (e -> e') -> Stream _ s e a -> EffKefir _ (Stream () s e' a)

    mapErrorsEff :: forall eff s e e' a. (e -> EffKefir eff e') -> Stream _ s e a -> EffKefir eff (Stream () s e' a)

    merge :: forall s e a. [Stream _ s e a] -> EffKefir _ (Stream () s e a)

    never :: EffKefir _ (Stream () T _ _)

    offLog :: Stream _ _ _ _ -> EffKefir _ Unit

    onAny :: forall eff e a. Stream _ _ e a -> (Event e a -> EffKefir eff _) -> EffKefir eff (Unregister eff)

    onEnd :: forall e. Stream _ (HasT _) _ _ -> EffKefir e _ -> EffKefir e (Unregister e)

    onError :: forall eff e. Stream _ (HasO _) e _ -> (e -> EffKefir eff _) -> EffKefir eff (Unregister eff)

    onLog :: Stream _ _ _ _ -> EffKefir _ Unit

    onLogWith :: Stream _ _ _ _ -> String -> EffKefir _ Unit

    onValue :: forall e a. Stream _ (HasO _) _ a -> (a -> EffKefir e _) -> EffKefir e (Unregister e)

    or :: forall s e. [Stream _ s e Boolean] -> EffKefir _ (Stream () s e Boolean)

    plug :: forall e a. Stream (HasP _) _ e a -> Stream _ _ e a -> EffKefir _ Unit

    pool :: forall eff e a. EffKefir eff (Stream P O e a)

    reduce :: forall s e a b. (b -> a -> b) -> b -> Stream _ (HasT s) e a -> EffKefir _ (Stream () (HasT s) e b)

    reduce1 :: forall s e a. (a -> a -> a) -> Stream _ (HasT s) e a -> EffKefir _ (Stream () (HasT s) e a)

    reduceEff :: forall eff s e a b. (b -> a -> EffKefir eff b) -> b -> Stream _ (HasT s) e a -> EffKefir eff (Stream () (HasT s) e b)

    reduceEff1 :: forall eff s e a. (a -> a -> EffKefir eff a) -> Stream _ (HasT s) e a -> EffKefir eff (Stream () (HasT s) e a)

    repeatedly :: forall a. Number -> [a] -> EffKefir _ (Stream () O _ a)

    sampledBy :: forall s e a b x. Passive _ _ e a -> Active _ s e b -> (a -> b -> x) -> EffKefir _ (Stream () s e x)

    scan :: forall s e a b. (b -> a -> b) -> b -> Stream _ s e a -> EffKefir _ (Property () s e b)

    scan1 :: forall s e a. (a -> a -> a) -> Stream _ s e a -> EffKefir _ (Property () s e a)

    sequentially :: forall a. Number -> [a] -> EffKefir _ (Stream () OT _ a)

    skip :: forall s e a. Number -> Stream _ s e a -> EffKefir _ (Stream () s e a)

    skipDuplicates :: forall e s a. (Eq a) => Stream _ s e a -> EffKefir _ (Stream () s e a)

    skipDuplicatesWith :: forall s e a. (a -> a -> Boolean) -> Stream _ s e a -> EffKefir _ (Stream () s e a)

    skipEnd :: forall stream s e a. stream _ (HasT s) e a -> EffKefir _ (stream () s e a)

    skipErrors :: forall s a. Stream _ s _ a -> EffKefir _ (Stream () s _ a)

    skipUntilBy :: forall s e a. Stream _ s e a -> Stream _ _ e _ -> EffKefir _ (Stream () s e a)

    skipValues :: forall s e. Stream _ s e _ -> EffKefir _ (Stream () s e _)

    skipWhile :: forall s e a. (a -> Boolean) -> Stream _ s e a -> EffKefir _ (Stream () s e a)

    skipWhileBy :: forall s e a. Stream _ s e a -> Stream _ _ e Boolean -> EffKefir _ (Stream () s e a)

    skipWhileEff :: forall eff s e a. (a -> EffKefir eff Boolean) -> Stream _ s e a -> EffKefir eff (Stream () s e a)

    slidingWindow :: forall stream s e a. Min -> Max -> stream _ s e a -> EffKefir _ (stream () s e [a])

    take :: forall e a. Number -> Stream _ _ e a -> EffKefir _ (Stream () OT e a)

    takeUntilBy :: forall e a. Stream _ _ e a -> Stream _ _ e _ -> EffKefir _ (Stream () OT e a)

    takeWhile :: forall e a. (a -> Boolean) -> Stream _ _ e a -> EffKefir _ (Stream () OT e a)

    takeWhileBy :: forall e a. Stream _ _ e a -> Stream _ _ e Boolean -> EffKefir _ (Stream () OT e a)

    takeWhileEff :: forall eff e a. (a -> EffKefir eff Boolean) -> Stream _ _ e a -> EffKefir eff (Stream () OT e a)

    throttle :: forall stream s e a. Number -> stream _ s e a -> EffKefir _ (stream () s e a)

    throttleWith :: forall stream s e a. { trailing :: Boolean, leading :: Boolean } -> Number -> stream _ s e a -> EffKefir _ (stream () s e a)

    toProperty :: forall s e a. Stream _ s e a -> EffKefir _ (Property () s e a)

    toPropertyWith :: forall s e a. a -> Stream _ s e a -> EffKefir _ (Property () s e a)

    unPlug :: forall e a. Stream (HasP _) _ e a -> Stream _ _ e a -> EffKefir _ Unit

    unsafeGlobalize :: forall p s e a. EffKefir _ (Stream p s e a) -> Stream p s e a

    valuesToErrors :: forall s e a. (a -> Maybe e) -> Stream _ s e a -> EffKefir _ (Stream () s e a)

    withHandler :: forall eff e e' a b. Stream _ _ e a -> (Stream E () e' b -> Event e a -> EffKefir eff _) -> EffKefir eff (Stream () OT e' b)

    withInterval :: forall e a. Number -> (Stream E () _ a -> EffKefir e Unit) -> EffKefir e (Stream () OT _ a)

    zipWith :: forall s e a b x. (a -> b -> x) -> Stream _ s e a -> Stream _ s e b -> EffKefir _ (Stream () s e x)



