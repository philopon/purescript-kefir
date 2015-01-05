# Module Documentation

## Module FRP.Kefir

### Types

    newtype Bus a

    newtype Constant a

    type EffKefir e = Eff (kefir :: Kefir | e)

    newtype Emitter a

    data Event a where
      Value :: Boolean -> a -> Event a
      End :: Event a

    newtype FromBinder a

    newtype FromCallback a

    newtype FromPoll a

    newtype Interval a

    data Kefir :: !

    newtype Later a

    type Max = Number

    type Min = Number

    newtype Never a

    newtype Pool a

    data Property :: * -> *

    newtype Repeatedly a

    newtype Sequentially a

    newtype SkipEnd a

    data Stream :: * -> *

    type Unregister e = EffKefir e Unit

    newtype WithInterval a


### Type Classes

    class (StreamLike stream) <= Emittable stream where

    class (StreamLike stream) <= IsProperty stream where

    class (StreamLike stream) <= IsStream stream where

    class (StreamLike stream) <= Observable stream where

    class (StreamLike stream) <= Pluggable stream where

    class StreamLike (stream :: * -> *) where

    class (StreamLike stream) <= Terminable stream where


### Type Class Instances

    instance emittableBus :: Emittable Bus

    instance emittableEmitter :: Emittable Emitter

    instance isPropertyConstant :: IsProperty Constant

    instance isPropertyProperty :: IsProperty Property

    instance isStreamBus :: IsStream Bus

    instance isStreamEmitter :: IsStream Emitter

    instance isStreamFromBinder :: IsStream FromBinder

    instance isStreamFromCallback :: IsStream FromCallback

    instance isStreamFromPoll :: IsStream FromPoll

    instance isStreamInterval :: IsStream Interval

    instance isStreamLater :: IsStream Later

    instance isStreamNever :: IsStream Never

    instance isStreamPool :: IsStream Pool

    instance isStreamRepeatedly :: IsStream Repeatedly

    instance isStreamSequentially :: IsStream Sequentially

    instance isStreamSkipEnd :: IsStream SkipEnd

    instance isStreamStream :: IsStream Stream

    instance isStreamWithInterval :: IsStream WithInterval

    instance observableBus :: Observable Bus

    instance observableConstant :: Observable Constant

    instance observableEmitter :: Observable Emitter

    instance observableFromBinder :: Observable FromBinder

    instance observableFromCallback :: Observable FromCallback

    instance observableFromPoll :: Observable FromPoll

    instance observableInterval :: Observable Interval

    instance observableLater :: Observable Later

    instance observablePool :: Observable Pool

    instance observableProperty :: Observable Property

    instance observableRepeatedly :: Observable Repeatedly

    instance observableSequentially :: Observable Sequentially

    instance observableSkipEnd :: Observable SkipEnd

    instance observableStream :: Observable Stream

    instance observableWithInterval :: Observable WithInterval

    instance pluggableBus :: Pluggable Bus

    instance pluggablePool :: Pluggable Pool

    instance streamLikeBus :: StreamLike Bus

    instance streamLikeConstant :: StreamLike Constant

    instance streamLikeEmitter :: StreamLike Emitter

    instance streamLikeFromBinder :: StreamLike FromBinder

    instance streamLikeFromCallback :: StreamLike FromCallback

    instance streamLikeFromPoll :: StreamLike FromPoll

    instance streamLikeInterval :: StreamLike Interval

    instance streamLikeLater :: StreamLike Later

    instance streamLikeNever :: StreamLike Never

    instance streamLikePool :: StreamLike Pool

    instance streamLikeProperty :: StreamLike Property

    instance streamLikeRepeatedly :: StreamLike Repeatedly

    instance streamLikeSequentially :: StreamLike Sequentially

    instance streamLikeSkipEnd :: StreamLike SkipEnd

    instance streamLikeStream :: StreamLike Stream

    instance streamLikeWithInterval :: StreamLike WithInterval

    instance terminableBus :: Terminable Bus

    instance terminableConstant :: Terminable Constant

    instance terminableEmitter :: Terminable Emitter

    instance terminableFromBinder :: Terminable FromBinder

    instance terminableFromCallback :: Terminable FromCallback

    instance terminableLater :: Terminable Later

    instance terminableNever :: Terminable Never

    instance terminableProperty :: Terminable Property

    instance terminableSequentially :: Terminable Sequentially

    instance terminableStream :: Terminable Stream

    instance terminableWithInterval :: Terminable WithInterval


### Values

    and :: forall stream e. (StreamLike stream) => [stream Boolean] -> EffKefir e (Stream Boolean)

    bufferWhile :: forall e stream a. (a -> Boolean) -> stream a -> EffKefir e (Stream [a])

    bufferWhileWith :: forall e stream a. { flushOnEnd :: Boolean } -> (a -> Boolean) -> stream a -> EffKefir e (Stream [a])

    bus :: forall e a. EffKefir e (Bus a)

    changes :: forall e stream a. (IsProperty stream) => stream a -> EffKefir e (Stream a)

    combine :: forall e streamA streamB a b x. (StreamLike streamA, StreamLike streamB) => streamA a -> streamB b -> (a -> b -> x) -> EffKefir e (Stream x)

    concat :: forall stream e a. (StreamLike stream) => [stream a] -> EffKefir e (Stream a)

    constant :: forall e a. a -> EffKefir e (Constant a)

    debounce :: forall e stream a. Number -> stream a -> EffKefir e (Stream a)

    debounceWith :: forall e stream a. { immediate :: Boolean } -> Number -> stream a -> EffKefir e (Stream a)

    delay :: forall e stream a. Number -> stream a -> EffKefir e (Stream a)

    diff :: forall e stream a b. (StreamLike stream) => (a -> a -> b) -> a -> stream a -> EffKefir e (Stream b)

    diff1 :: forall e stream a b. (StreamLike stream) => (a -> a -> b) -> stream a -> EffKefir e (Stream b)

    emit :: forall e stream a. (Emittable stream) => stream a -> a -> EffKefir e Unit

    emitter :: forall e a. EffKefir e (Emitter a)

    end :: forall e stream. (Emittable stream) => stream _ -> EffKefir e Unit

    filter :: forall e stream a. (StreamLike stream) => (a -> Boolean) -> stream a -> EffKefir e (Stream a)

    filterEff :: forall e stream a. (StreamLike stream) => (a -> EffKefir e Boolean) -> stream a -> EffKefir e (Stream a)

    flatMap :: forall e stream child a. (StreamLike stream, StreamLike child) => stream (child a) -> EffKefir e (Stream a)

    flatMapConcat :: forall e stream child a. (StreamLike stream, StreamLike child) => stream (child a) -> EffKefir e (Stream a)

    flatMapConcatWith :: forall e stream child a b. (StreamLike stream, StreamLike child) => stream a -> (a -> EffKefir e (child b)) -> EffKefir e (Stream b)

    flatMapConcurLimit :: forall e stream child a. (StreamLike stream, StreamLike child) => Number -> stream (child a) -> EffKefir e (Stream a)

    flatMapConcurLimitWith :: forall e stream child a b. (StreamLike stream, StreamLike child) => Number -> stream a -> (a -> EffKefir e (child b)) -> EffKefir e (Stream b)

    flatMapFirst :: forall e stream child a. (StreamLike stream, StreamLike child) => stream (child a) -> EffKefir e (Stream a)

    flatMapFirstWith :: forall e stream child a b. (StreamLike stream, StreamLike child) => stream a -> (a -> EffKefir e (child b)) -> EffKefir e (Stream b)

    flatMapLatest :: forall e stream child a. (StreamLike stream, StreamLike child) => stream (child a) -> EffKefir e (Stream a)

    flatMapLatestWith :: forall e stream child a b. (StreamLike stream, StreamLike child) => stream a -> (a -> EffKefir e (child b)) -> EffKefir e (Stream b)

    flatMapWith :: forall e stream child a b. (StreamLike stream, StreamLike child) => stream a -> (a -> EffKefir e (child b)) -> EffKefir e (Stream b)

    flatten :: forall e stream a. stream [a] -> EffKefir e (Stream a)

    flattenWith :: forall e stream a b. (a -> [b]) -> stream a -> EffKefir e (Stream b)

    forget :: forall stream a. (StreamLike stream) => stream a -> Stream a

    fromBinder :: forall e a. (Emitter a -> EffKefir e (EffKefir e Unit)) -> EffKefir e (FromBinder a)

    fromCallback :: forall e a. ((a -> EffKefir e Unit) -> EffKefir e Unit) -> EffKefir e (FromCallback a)

    fromPoll :: forall e a. Number -> EffKefir e a -> EffKefir e (FromPoll a)

    interval :: forall e a. Number -> a -> EffKefir e (Interval a)

    later :: forall e a. Number -> a -> EffKefir e (Later a)

    map :: forall e stream a b. (StreamLike stream) => (a -> b) -> stream a -> EffKefir e (Stream b)

    mapEff :: forall e stream a b. (StreamLike stream) => (a -> EffKefir e b) -> stream a -> EffKefir e (Stream b)

    mapEnd :: forall e stream a. EffKefir e a -> stream a -> EffKefir e (Stream a)

    merge :: forall stream e a. (StreamLike stream) => [stream a] -> EffKefir e (Stream a)

    never :: forall e a. EffKefir e (Never a)

    offLog :: forall e stream. (StreamLike stream) => stream _ -> EffKefir e Unit

    onAny :: forall e stream a. (Terminable stream, Observable stream) => stream a -> (Event a -> EffKefir e _) -> EffKefir e (Unregister e)

    onEnd :: forall e stream a. (Terminable stream) => stream a -> EffKefir e _ -> EffKefir e (Unregister e)

    onLog :: forall e stream. (StreamLike stream) => stream _ -> String -> EffKefir e Unit

    onValue :: forall e stream a. (Observable stream) => stream a -> (a -> EffKefir e _) -> EffKefir e (Unregister e)

    or :: forall stream e. (StreamLike stream) => [stream Boolean] -> EffKefir e (Stream Boolean)

    plug :: forall e hub stream a. (Pluggable hub, StreamLike stream) => hub a -> stream a -> EffKefir e Unit

    pool :: forall e a. EffKefir e (Pool a)

    reduce :: forall e stream a b. (StreamLike stream) => (b -> a -> b) -> b -> stream a -> EffKefir e (Stream b)

    reduce1 :: forall e stream a. (StreamLike stream) => (a -> a -> a) -> stream a -> EffKefir e (Stream a)

    reduceEff :: forall e stream a b. (StreamLike stream) => (b -> a -> EffKefir e b) -> b -> stream a -> EffKefir e (Stream b)

    reduceEff1 :: forall e stream a. (StreamLike stream) => (a -> a -> EffKefir e a) -> stream a -> EffKefir e (Stream a)

    repeatedly :: forall e a. Number -> [a] -> EffKefir e (Repeatedly a)

    sampledBy :: forall e passive active a b x. (StreamLike passive, StreamLike active) => passive a -> active b -> (a -> b -> x) -> EffKefir e (Stream x)

    scan :: forall e stream a b. (StreamLike stream) => (b -> a -> b) -> b -> stream a -> EffKefir e (Stream b)

    scan1 :: forall e stream a. (StreamLike stream) => (a -> a -> a) -> stream a -> EffKefir e (Stream a)

    sequentially :: forall e a. Number -> [a] -> EffKefir e (Sequentially a)

    skip :: forall e stream a. (StreamLike stream) => Number -> stream a -> EffKefir e (Stream a)

    skipDuplicates :: forall e stream a. (StreamLike stream, Eq a) => stream a -> EffKefir e (Stream a)

    skipDuplicatesWith :: forall e stream a. (StreamLike stream) => (a -> a -> Boolean) -> stream a -> EffKefir e (Stream a)

    skipEnd :: forall e stream a. stream a -> EffKefir e (SkipEnd a)

    skipWhile :: forall e stream a. (StreamLike stream) => (a -> Boolean) -> stream a -> EffKefir e (Stream a)

    skipWhileEff :: forall e stream a. (StreamLike stream) => (a -> EffKefir e Boolean) -> stream a -> EffKefir e (Stream a)

    slidingWindow :: forall e stream a. Min -> Max -> stream a -> EffKefir e (Stream [a])

    take :: forall e stream a. (StreamLike stream) => Number -> stream a -> EffKefir e (Stream a)

    takeWhile :: forall e stream a. (StreamLike stream) => (a -> Boolean) -> stream a -> EffKefir e (Stream a)

    takeWhileEff :: forall e stream a. (StreamLike stream) => (a -> EffKefir e Boolean) -> stream a -> EffKefir e (Stream a)

    throttle :: forall e stream a. Number -> stream a -> EffKefir e (Stream a)

    throttleWith :: forall e stream a. { trailing :: Boolean, leading :: Boolean } -> Number -> stream a -> EffKefir e (Stream a)

    toProperty :: forall e stream a. (IsStream stream) => stream a -> EffKefir e (Property a)

    toPropertyWith :: forall e stream a. (IsStream stream) => a -> stream a -> EffKefir e (Property a)

    unPlug :: forall e hub stream a. (Pluggable hub, StreamLike stream) => hub a -> stream a -> EffKefir e Unit

    withDefault :: forall e stream a. (StreamLike stream) => a -> stream a -> EffKefir e (Property a)

    withHandler :: forall e stream a b. (StreamLike stream) => stream a -> (Emitter b -> Event a -> EffKefir e _) -> EffKefir e (Stream b)

    withInterval :: forall e a. Number -> (Emitter a -> EffKefir e Unit) -> EffKefir e (WithInterval a)

    zipWith :: forall e streamA streamB a b x. (StreamLike streamA, StreamLike streamB) => (a -> b -> x) -> streamA a -> streamB b -> EffKefir e (Stream x)



