# Module Documentation

## Module FRP.Kefir

### Types

    type E = HasE ()

    type EP = (pluggable :: Pluggable, emittable :: Emittable)

    type EffKefir e = Eff (kefir :: Kefir | e)

    data Emittable :: *

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

    type OT = (terminable :: Terminable, observable :: Observable)

    data Observable :: *

    type P = HasP ()

    data Pluggable :: *

    data Property :: # * -> # * -> * -> *

    data Stream :: # * -> # * -> * -> *

    type T = HasT ()

    data Terminable :: *

    type Unregister e = EffKefir e Unit


### Type Classes

    class StreamLike (stream :: # * -> # * -> * -> *) where


### Type Class Instances

    instance streamLikeProperty :: StreamLike Property

    instance streamLikeStream :: StreamLike Stream


### Values

    and :: forall stream s. (StreamLike stream) => [stream _ s Boolean] -> EffKefir _ (Stream () s Boolean)

    awaiting :: forall on off s. (StreamLike on, StreamLike off) => on _ s _ -> off _ s _ -> EffKefir _ (Property () s Boolean)

    bufferBy :: forall stream filter s a. (StreamLike stream, StreamLike filter) => stream _ s a -> filter _ _ _ -> EffKefir _ (stream () s [a])

    bufferWhile :: forall stream s a. (a -> Boolean) -> stream _ s a -> EffKefir _ (stream () s [a])

    bufferWhileBy :: forall stream filter s a. (StreamLike stream, StreamLike filter) => stream _ s a -> filter _ _ Boolean -> EffKefir _ (stream () s [a])

    bufferWhileWith :: forall stream s a. { flushOnEnd :: Boolean } -> (a -> Boolean) -> stream _ s a -> EffKefir _ (stream () s [a])

    bus :: forall e a. EffKefir e (Stream EP OT a)

    changes :: forall s a. Property _ s a -> EffKefir _ (Stream () s a)

    combine :: forall streamA streamB a b x. (StreamLike streamA, StreamLike streamB) => streamA _ _ a -> streamB _ _ b -> (a -> b -> x) -> EffKefir _ (Stream () OT x)

    concat :: forall stream s a. (StreamLike stream) => [stream _ s a] -> EffKefir _ (Stream () s a)

    constant :: forall a. a -> EffKefir _ (Property () OT a)

    debounce :: forall stream s a. Number -> stream _ s a -> EffKefir _ (stream () s a)

    debounceWith :: forall stream s a. { immediate :: Boolean } -> Number -> stream _ s a -> EffKefir _ (stream () s a)

    delay :: forall stream s a. Number -> stream _ s a -> EffKefir _ (stream () s a)

    diff :: forall stream s a b. (StreamLike stream) => (a -> a -> b) -> a -> stream _ s a -> EffKefir _ (stream () s b)

    diff1 :: forall stream s a b. (StreamLike stream) => (a -> a -> b) -> stream _ s a -> EffKefir _ (stream () s b)

    emit :: forall a. Stream (HasE _) _ a -> a -> EffKefir _ Unit

    emitAsync :: forall a. Stream (HasE _) _ a -> a -> EffKefir _ Unit

    emitter :: EffKefir _ (Stream E OT _)

    end :: Stream (HasE _) _ _ -> EffKefir _ Unit

    endAsync :: Stream (HasE _) _ _ -> EffKefir _ Unit

    filter :: forall stream s a. (StreamLike stream) => (a -> Boolean) -> stream _ s a -> EffKefir _ (stream () s a)

    filterBy :: forall stream filter s a. (StreamLike stream, StreamLike filter) => stream _ s a -> filter _ _ Boolean -> EffKefir _ (stream () s a)

    filterEff :: forall e stream s a. (StreamLike stream) => (a -> EffKefir e Boolean) -> stream _ s a -> EffKefir e (stream () s a)

    flatMap :: forall stream child a. (StreamLike stream, StreamLike child) => stream _ _ (child _ _ a) -> EffKefir _ (Stream () OT a)

    flatMapConcat :: forall stream child a. (StreamLike stream, StreamLike child) => stream _ _ (child _ _ a) -> EffKefir _ (Stream () OT a)

    flatMapConcatWith :: forall e stream child a b. (StreamLike stream, StreamLike child) => stream _ _ a -> (a -> EffKefir e (child _ _ b)) -> EffKefir e (Stream () OT b)

    flatMapConcurLimit :: forall stream child a. (StreamLike stream, StreamLike child) => Number -> stream _ _ (child _ _ a) -> EffKefir _ (Stream () OT a)

    flatMapConcurLimitWith :: forall e stream child a b. (StreamLike stream, StreamLike child) => Number -> stream _ _ a -> (a -> EffKefir e (child _ _ b)) -> EffKefir e (Stream () OT b)

    flatMapFirst :: forall stream child a. (StreamLike stream, StreamLike child) => stream _ _ (child _ _ a) -> EffKefir _ (Stream () OT a)

    flatMapFirstWith :: forall e stream child a b. (StreamLike stream, StreamLike child) => stream _ _ a -> (a -> EffKefir e (child _ _ b)) -> EffKefir e (Stream () OT b)

    flatMapLatest :: forall stream child a. (StreamLike stream, StreamLike child) => stream _ _ (child _ _ a) -> EffKefir _ (Stream () OT a)

    flatMapLatestWith :: forall e stream child a b. (StreamLike stream, StreamLike child) => stream _ _ a -> (a -> EffKefir e (child _ _ b)) -> EffKefir e (Stream () OT b)

    flatMapWith :: forall e stream child a b. (StreamLike stream, StreamLike child) => stream _ _ a -> (a -> EffKefir e (child _ _ b)) -> EffKefir e (Stream () OT b)

    flatten :: forall stream s a. stream _ s [a] -> EffKefir _ (stream () s a)

    flattenWith :: forall stream s a b. (a -> [b]) -> stream _ s a -> EffKefir _ (stream () s b)

    forget :: forall stream a. (StreamLike stream) => stream _ _ a -> Stream _ _ a

    fromBinder :: forall e a. (Stream E () a -> EffKefir e (EffKefir e Unit)) -> EffKefir e (Stream () OT a)

    fromCallback :: forall e a. ((a -> EffKefir e Unit) -> EffKefir e Unit) -> EffKefir e (Stream () OT a)

    fromPoll :: forall e a. Number -> EffKefir e a -> EffKefir e (Stream () O a)

    interval :: forall a. Number -> a -> EffKefir _ (Stream () O a)

    later :: forall a. Number -> a -> EffKefir _ (Stream () OT a)

    map :: forall stream s a b. (StreamLike stream) => (a -> b) -> stream _ s a -> EffKefir _ (stream () s b)

    mapEff :: forall e stream s a b. (StreamLike stream) => (a -> EffKefir e b) -> stream _ s a -> EffKefir e (stream () s b)

    mapEnd :: forall e stream s a. EffKefir e a -> stream _ (HasT s) a -> EffKefir e (stream () OT a)

    merge :: forall stream s a. (StreamLike stream) => [stream _ s a] -> EffKefir _ (Stream () s a)

    never :: EffKefir _ (Stream () T _)

    offLog :: forall stream. (StreamLike stream) => stream _ _ _ -> EffKefir _ Unit

    onAny :: forall e stream a. (StreamLike stream) => stream _ _ a -> (Event a -> EffKefir e _) -> EffKefir e (Unregister e)

    onEnd :: forall e stream. (StreamLike stream) => stream _ (HasT _) _ -> EffKefir e _ -> EffKefir e (Unregister e)

    onLog :: forall stream. (StreamLike stream) => stream _ _ _ -> String -> EffKefir _ Unit

    onValue :: forall e stream a. (StreamLike stream) => stream _ (HasO _) a -> (a -> EffKefir e _) -> EffKefir e (Unregister e)

    or :: forall stream s. (StreamLike stream) => [stream _ s Boolean] -> EffKefir _ (Stream () s Boolean)

    plug :: forall stream a. (StreamLike stream) => Stream (HasP _) _ a -> stream _ _ a -> EffKefir _ Unit

    pool :: forall e a. EffKefir e (Stream P O a)

    reduce :: forall stream s a b. (StreamLike stream) => (b -> a -> b) -> b -> stream _ (HasT s) a -> EffKefir _ (stream () (HasT s) b)

    reduce1 :: forall stream s a. (StreamLike stream) => (a -> a -> a) -> stream _ (HasT s) a -> EffKefir _ (stream () (HasT s) a)

    reduceEff :: forall e stream s a b. (StreamLike stream) => (b -> a -> EffKefir e b) -> b -> stream _ (HasT s) a -> EffKefir e (stream () (HasT s) b)

    reduceEff1 :: forall e stream s a. (StreamLike stream) => (a -> a -> EffKefir e a) -> stream _ (HasT s) a -> EffKefir e (stream () (HasT s) a)

    repeatedly :: forall a. Number -> [a] -> EffKefir _ (Stream () O a)

    sampledBy :: forall passive active s a b x. (StreamLike passive, StreamLike active) => passive _ _ a -> active _ s b -> (a -> b -> x) -> EffKefir _ (Stream () s x)

    scan :: forall stream s a b. (StreamLike stream) => (b -> a -> b) -> b -> stream _ s a -> EffKefir _ (Property () s b)

    scan1 :: forall stream s a. (StreamLike stream) => (a -> a -> a) -> stream _ s a -> EffKefir _ (Property () s a)

    sequentially :: forall a. Number -> [a] -> EffKefir _ (Stream () OT a)

    skip :: forall stream s a. (StreamLike stream) => Number -> stream _ s a -> EffKefir _ (stream () s a)

    skipDuplicates :: forall stream s a. (StreamLike stream, Eq a) => stream _ s a -> EffKefir _ (stream () s a)

    skipDuplicatesWith :: forall stream s a. (StreamLike stream) => (a -> a -> Boolean) -> stream _ s a -> EffKefir _ (stream () s a)

    skipEnd :: forall stream s a. stream _ (HasT s) a -> EffKefir _ (stream () s a)

    skipUntilBy :: forall stream filter s a. (StreamLike stream, StreamLike filter) => stream _ s a -> filter _ _ _ -> EffKefir _ (stream () s a)

    skipWhile :: forall stream s a. (StreamLike stream) => (a -> Boolean) -> stream _ s a -> EffKefir _ (stream () s a)

    skipWhileBy :: forall stream filter s a. (StreamLike stream, StreamLike filter) => stream _ s a -> filter _ _ Boolean -> EffKefir _ (stream () s a)

    skipWhileEff :: forall e stream s a. (StreamLike stream) => (a -> EffKefir e Boolean) -> stream _ s a -> EffKefir e (stream () s a)

    slidingWindow :: forall stream s a. Min -> Max -> stream _ s a -> EffKefir _ (stream () s [a])

    take :: forall stream a. (StreamLike stream) => Number -> stream _ _ a -> EffKefir _ (stream () OT a)

    takeUntilBy :: forall stream filter a. (StreamLike stream, StreamLike filter) => stream _ _ a -> filter _ _ _ -> EffKefir _ (stream () OT a)

    takeWhile :: forall stream a. (StreamLike stream) => (a -> Boolean) -> stream _ _ a -> EffKefir _ (stream () OT a)

    takeWhileBy :: forall stream filter a. (StreamLike stream, StreamLike filter) => stream _ _ a -> filter _ _ Boolean -> EffKefir _ (stream () OT a)

    takeWhileEff :: forall e stream a. (StreamLike stream) => (a -> EffKefir e Boolean) -> stream _ _ a -> EffKefir e (stream () OT a)

    throttle :: forall stream s a. Number -> stream _ s a -> EffKefir _ (stream () s a)

    throttleWith :: forall stream s a. { trailing :: Boolean, leading :: Boolean } -> Number -> stream _ s a -> EffKefir _ (stream () s a)

    toProperty :: forall s a. Stream _ s a -> EffKefir _ (Property () s a)

    toPropertyWith :: forall s a. a -> Stream _ s a -> EffKefir _ (Property () s a)

    unPlug :: forall stream a. (StreamLike stream) => Stream (HasP _) _ a -> stream _ _ a -> EffKefir _ Unit

    unsafeGlobalize :: forall stream p s a. (StreamLike stream) => EffKefir _ (stream p s a) -> stream p s a

    withDefault :: forall stream s a. (StreamLike stream) => a -> stream _ s a -> EffKefir _ (Property () s a)

    withHandler :: forall e stream a b. (StreamLike stream) => stream _ _ a -> (Stream E () b -> Event a -> EffKefir e _) -> EffKefir e (stream () OT b)

    withInterval :: forall e a. Number -> (Stream E () a -> EffKefir e Unit) -> EffKefir e (Stream () OT a)

    zipWith :: forall streamA streamB a b s x. (StreamLike streamA, StreamLike streamB) => (a -> b -> x) -> streamA _ s a -> streamB _ s b -> EffKefir _ (Stream () s x)



