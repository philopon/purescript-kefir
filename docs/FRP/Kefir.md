# Module Documentation

## Module FRP.Kefir

### Types

    type E = HasE ()

    type EOPT = (terminable :: Terminable, pluggable :: Pluggable, observable :: Observable, emittable :: Emittable)

    type EOT = (terminable :: Terminable, observable :: Observable, emittable :: Emittable)

    type EffKefir e = Eff (kefir :: Kefir | e)

    data Event a where
      Value :: Boolean -> a -> Event a
      End :: Event a

    type HasE s = (emittable :: Emittable | s)

    type HasO s = (observable :: Observable | s)

    type HasP s = (pluggable :: Pluggable | s)

    type HasT s = (terminable :: Terminable | s)

    data Kefir :: !

    type Max = Number

    type Min = Number

    type O = HasO ()

    type OP = (pluggable :: Pluggable, observable :: Observable)

    type OT = (terminable :: Terminable, observable :: Observable)

    type P = HasP ()

    data Property :: # * -> * -> *

    data Stream :: # * -> * -> *

    type T = HasT ()

    type Unregister e = EffKefir e Unit


### Type Classes

    class StreamLike (stream :: # * -> * -> *) where


### Type Class Instances

    instance streamLikeProperty :: StreamLike Property

    instance streamLikeStream :: StreamLike Stream


### Values

    and :: forall stream s. (StreamLike stream) => [stream s Boolean] -> EffKefir _ (Stream s Boolean)

    awaiting :: forall on off s. (StreamLike on, StreamLike off) => on s _ -> off s _ -> EffKefir _ (Stream s Boolean)

    bufferBy :: forall stream filter s a. (StreamLike stream, StreamLike filter) => stream s a -> filter _ _ -> EffKefir _ (Stream s [a])

    bufferWhile :: forall stream s a. (a -> Boolean) -> stream s a -> EffKefir _ (Stream s [a])

    bufferWhileBy :: forall stream filter s a. (StreamLike stream, StreamLike filter) => stream s a -> filter _ Boolean -> EffKefir _ (Stream s [a])

    bufferWhileWith :: forall stream s a. { flushOnEnd :: Boolean } -> (a -> Boolean) -> stream s a -> EffKefir _ (Stream s [a])

    bus :: forall e a. EffKefir e (Stream EOPT a)

    changes :: forall s a. Property s a -> EffKefir _ (Stream s a)

    combine :: forall streamA streamB a b x. (StreamLike streamA, StreamLike streamB) => streamA _ a -> streamB _ b -> (a -> b -> x) -> EffKefir _ (Stream OT x)

    concat :: forall stream s a. (StreamLike stream) => [stream s a] -> EffKefir _ (Stream s a)

    constant :: forall a. a -> EffKefir _ (Property OT a)

    debounce :: forall stream s a. Number -> stream s a -> EffKefir _ (Stream s a)

    debounceWith :: forall stream s a. { immediate :: Boolean } -> Number -> stream s a -> EffKefir _ (Stream s a)

    delay :: forall stream s a. Number -> stream s a -> EffKefir _ (Stream s a)

    diff :: forall stream s a b. (StreamLike stream) => (a -> a -> b) -> a -> stream s a -> EffKefir _ (Stream s b)

    diff1 :: forall stream s a b. (StreamLike stream) => (a -> a -> b) -> stream s a -> EffKefir _ (Stream s b)

    emit :: forall a. Stream (HasE _) a -> a -> EffKefir _ Unit

    emitter :: EffKefir _ (Stream EOT _)

    end :: Stream (HasE _) _ -> EffKefir _ Unit

    filter :: forall stream s a. (StreamLike stream) => (a -> Boolean) -> stream s a -> EffKefir _ (Stream s a)

    filterBy :: forall stream filter s a. (StreamLike stream, StreamLike filter) => stream s a -> filter _ Boolean -> EffKefir _ (Stream s a)

    filterEff :: forall e stream s a. (StreamLike stream) => (a -> EffKefir e Boolean) -> stream s a -> EffKefir e (Stream s a)

    flatMap :: forall stream child a. (StreamLike stream, StreamLike child) => stream _ (child _ a) -> EffKefir _ (Stream OT a)

    flatMapConcat :: forall stream child a. (StreamLike stream, StreamLike child) => stream _ (child _ a) -> EffKefir _ (Stream OT a)

    flatMapConcatWith :: forall e stream child a b. (StreamLike stream, StreamLike child) => stream _ a -> (a -> EffKefir e (child _ b)) -> EffKefir e (Stream OT b)

    flatMapConcurLimit :: forall stream child a. (StreamLike stream, StreamLike child) => Number -> stream _ (child _ a) -> EffKefir _ (Stream OT a)

    flatMapConcurLimitWith :: forall e stream child a b. (StreamLike stream, StreamLike child) => Number -> stream _ a -> (a -> EffKefir e (child _ b)) -> EffKefir e (Stream OT b)

    flatMapFirst :: forall stream child a. (StreamLike stream, StreamLike child) => stream _ (child _ a) -> EffKefir _ (Stream OT a)

    flatMapFirstWith :: forall e stream child a b. (StreamLike stream, StreamLike child) => stream _ a -> (a -> EffKefir e (child _ b)) -> EffKefir e (Stream OT b)

    flatMapLatest :: forall stream child a. (StreamLike stream, StreamLike child) => stream _ (child _ a) -> EffKefir _ (Stream OT a)

    flatMapLatestWith :: forall e stream child a b. (StreamLike stream, StreamLike child) => stream _ a -> (a -> EffKefir e (child _ b)) -> EffKefir e (Stream OT b)

    flatMapWith :: forall e stream child a b. (StreamLike stream, StreamLike child) => stream _ a -> (a -> EffKefir e (child _ b)) -> EffKefir e (Stream OT b)

    flatten :: forall stream s a. stream s [a] -> EffKefir _ (Stream s a)

    flattenWith :: forall stream s a b. (a -> [b]) -> stream s a -> EffKefir _ (Stream s b)

    forget :: forall stream a. (StreamLike stream) => stream _ a -> Stream _ a

    fromBinder :: forall e a. (Stream E a -> EffKefir e (EffKefir e Unit)) -> EffKefir e (Stream OT a)

    fromCallback :: forall e a. ((a -> EffKefir e Unit) -> EffKefir e Unit) -> EffKefir e (Stream OT a)

    fromPoll :: forall e a. Number -> EffKefir e a -> EffKefir e (Stream O a)

    interval :: forall a. Number -> a -> EffKefir _ (Stream O a)

    later :: forall a. Number -> a -> EffKefir _ (Stream OT a)

    map :: forall stream s a b. (StreamLike stream) => (a -> b) -> stream s a -> EffKefir _ (Stream s b)

    mapEff :: forall e stream s a b. (StreamLike stream) => (a -> EffKefir e b) -> stream s a -> EffKefir e (Stream s b)

    mapEnd :: forall e stream s a. EffKefir e a -> stream (HasT s) a -> EffKefir e (Stream OT a)

    merge :: forall stream s a. (StreamLike stream) => [stream s a] -> EffKefir _ (Stream s a)

    never :: EffKefir _ (Stream T _)

    offLog :: forall stream. (StreamLike stream) => stream _ _ -> EffKefir _ Unit

    onAny :: forall e stream a. (StreamLike stream) => stream _ a -> (Event a -> EffKefir e _) -> EffKefir e (Unregister e)

    onEnd :: forall e stream. (StreamLike stream) => stream (HasT _) _ -> EffKefir e _ -> EffKefir e (Unregister e)

    onLog :: forall stream. (StreamLike stream) => stream _ _ -> String -> EffKefir _ Unit

    onValue :: forall e stream a. (StreamLike stream) => stream (HasO _) a -> (a -> EffKefir e _) -> EffKefir e (Unregister e)

    or :: forall stream s. (StreamLike stream) => [stream s Boolean] -> EffKefir _ (Stream s Boolean)

    plug :: forall stream a. (StreamLike stream) => Stream (HasP _) a -> stream _ a -> EffKefir _ Unit

    pool :: forall e a. EffKefir e (Stream OP a)

    reduce :: forall stream s a b. (StreamLike stream) => (b -> a -> b) -> b -> stream (HasT s) a -> EffKefir _ (Stream (HasT s) b)

    reduce1 :: forall stream s a. (StreamLike stream) => (a -> a -> a) -> stream (HasT s) a -> EffKefir _ (Stream (HasT s) a)

    reduceEff :: forall e stream s a b. (StreamLike stream) => (b -> a -> EffKefir e b) -> b -> stream (HasT s) a -> EffKefir e (Stream (HasT s) b)

    reduceEff1 :: forall e stream s a. (StreamLike stream) => (a -> a -> EffKefir e a) -> stream (HasT s) a -> EffKefir e (Stream (HasT s) a)

    repeatedly :: forall a. Number -> [a] -> EffKefir _ (Stream O a)

    sampledBy :: forall passive active s a b x. (StreamLike passive, StreamLike active) => passive _ a -> active s b -> (a -> b -> x) -> EffKefir _ (Stream s x)

    scan :: forall stream s a b. (StreamLike stream) => (b -> a -> b) -> b -> stream s a -> EffKefir _ (Stream s b)

    scan1 :: forall stream s a. (StreamLike stream) => (a -> a -> a) -> stream s a -> EffKefir _ (Stream s a)

    sequentially :: forall a. Number -> [a] -> EffKefir _ (Stream OT a)

    skip :: forall stream s a. (StreamLike stream) => Number -> stream s a -> EffKefir _ (Stream s a)

    skipDuplicates :: forall stream s a. (StreamLike stream, Eq a) => stream s a -> EffKefir _ (Stream s a)

    skipDuplicatesWith :: forall stream s a. (StreamLike stream) => (a -> a -> Boolean) -> stream s a -> EffKefir _ (Stream s a)

    skipEnd :: forall stream s a. stream (HasT s) a -> EffKefir _ (Stream s a)

    skipUntilBy :: forall stream filter s a. (StreamLike stream, StreamLike filter) => stream s a -> filter _ _ -> EffKefir _ (Stream s a)

    skipWhile :: forall stream s a. (StreamLike stream) => (a -> Boolean) -> stream s a -> EffKefir _ (Stream s a)

    skipWhileBy :: forall stream filter s a. (StreamLike stream, StreamLike filter) => stream s a -> filter _ Boolean -> EffKefir _ (Stream s a)

    skipWhileEff :: forall e stream s a. (StreamLike stream) => (a -> EffKefir e Boolean) -> stream s a -> EffKefir e (Stream s a)

    slidingWindow :: forall stream s a. Min -> Max -> stream s a -> EffKefir _ (Stream s [a])

    take :: forall stream s a. (StreamLike stream) => Number -> stream s a -> EffKefir _ (Stream OT a)

    takeUntilBy :: forall stream filter a. (StreamLike stream, StreamLike filter) => stream _ a -> filter _ _ -> EffKefir _ (Stream OT a)

    takeWhile :: forall stream s a. (StreamLike stream) => (a -> Boolean) -> stream s a -> EffKefir _ (Stream OT a)

    takeWhileBy :: forall stream filter a. (StreamLike stream, StreamLike filter) => stream _ a -> filter _ Boolean -> EffKefir _ (Stream OT a)

    takeWhileEff :: forall e stream s a. (StreamLike stream) => (a -> EffKefir e Boolean) -> stream s a -> EffKefir e (Stream OT a)

    throttle :: forall stream s a. Number -> stream s a -> EffKefir _ (Stream s a)

    throttleWith :: forall stream s a. { trailing :: Boolean, leading :: Boolean } -> Number -> stream s a -> EffKefir _ (Stream s a)

    toProperty :: forall s a. Stream s a -> EffKefir _ (Property s a)

    toPropertyWith :: forall s a. a -> Stream s a -> EffKefir _ (Property s a)

    unPlug :: forall stream a. (StreamLike stream) => Stream (HasP _) a -> stream _ a -> EffKefir _ Unit

    withDefault :: forall stream s a. (StreamLike stream) => a -> stream s a -> EffKefir _ (Property s a)

    withHandler :: forall e stream s a b. (StreamLike stream) => stream s a -> (Stream E b -> Event a -> EffKefir e _) -> EffKefir e (Stream OT b)

    withInterval :: forall e a. Number -> (Stream E a -> EffKefir e Unit) -> EffKefir e (Stream OT a)

    zipWith :: forall streamA streamB a b s x. (StreamLike streamA, StreamLike streamB) => (a -> b -> x) -> streamA s a -> streamB s b -> EffKefir _ (Stream s x)



