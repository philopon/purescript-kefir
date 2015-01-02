# Module Documentation

## Module FRP.Kefir

### Types

    newtype Constant a

    type EffKefir e = Eff (kefir :: Kefir | e)

    newtype Emitter a

    newtype FromBinder a

    newtype FromCallback a

    newtype FromPoll a

    newtype FunKey

    newtype Interval a

    data Kefir :: !

    newtype Later a

    newtype Never a

    data Property :: * -> *

    newtype Repeatedly a

    newtype Sequentially a

    data Stream :: * -> *

    newtype WithInterval a


### Type Classes

    class (StreamLike stream) <= IsProperty stream where

    class (StreamLike stream) <= IsStream stream where

    class (StreamLike stream) <= Observable stream where

    class StreamLike (stream :: * -> *) where

    class (StreamLike stream) <= Terminable stream where


### Type Class Instances

    instance isPropertyConstant :: IsProperty Constant

    instance isPropertyProperty :: IsProperty Property

    instance isStreamEmitter :: IsStream Emitter

    instance isStreamFromBinder :: IsStream FromBinder

    instance isStreamFromCallback :: IsStream FromCallback

    instance isStreamFromPoll :: IsStream FromPoll

    instance isStreamInterval :: IsStream Interval

    instance isStreamLater :: IsStream Later

    instance isStreamNever :: IsStream Never

    instance isStreamRepeatedly :: IsStream Repeatedly

    instance isStreamSequentially :: IsStream Sequentially

    instance isStreamStream :: IsStream Stream

    instance isStreamWithInterval :: IsStream WithInterval

    instance observableConstant :: Observable Constant

    instance observableEmitter :: Observable Emitter

    instance observableFromBinder :: Observable FromBinder

    instance observableFromCallback :: Observable FromCallback

    instance observableFromPoll :: Observable FromPoll

    instance observableInterval :: Observable Interval

    instance observableLater :: Observable Later

    instance observableProperty :: Observable Property

    instance observableRepeatedly :: Observable Repeatedly

    instance observableSequentially :: Observable Sequentially

    instance observableStream :: Observable Stream

    instance observableWithInterval :: Observable WithInterval

    instance streamLikeConstant :: StreamLike Constant

    instance streamLikeEmitter :: StreamLike Emitter

    instance streamLikeFromBinder :: StreamLike FromBinder

    instance streamLikeFromCallback :: StreamLike FromCallback

    instance streamLikeFromPoll :: StreamLike FromPoll

    instance streamLikeInterval :: StreamLike Interval

    instance streamLikeLater :: StreamLike Later

    instance streamLikeNever :: StreamLike Never

    instance streamLikeProperty :: StreamLike Property

    instance streamLikeRepeatedly :: StreamLike Repeatedly

    instance streamLikeSequentially :: StreamLike Sequentially

    instance streamLikeStream :: StreamLike Stream

    instance streamLikeWithInterval :: StreamLike WithInterval

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

    changes :: forall e stream a. (IsProperty stream) => stream a -> EffKefir e (Stream a)

    constant :: forall e a. a -> EffKefir e (Constant a)

    emit :: forall e a. Emitter a -> a -> EffKefir e Unit

    emitter :: forall e a. EffKefir e (Emitter a)

    end :: forall e a. Emitter a -> EffKefir e Unit

    filter :: forall e stream a. (StreamLike stream) => (a -> Boolean) -> stream a -> EffKefir e (Stream a)

    filterEff :: forall e stream a. (StreamLike stream) => (a -> EffKefir e Boolean) -> stream a -> EffKefir e (Stream a)

    fromBinder :: forall e a. (Emitter a -> EffKefir e (EffKefir e Unit)) -> EffKefir e (FromBinder a)

    fromCallback :: forall e a. ((a -> EffKefir e Unit) -> EffKefir e Unit) -> EffKefir e (FromCallback a)

    fromPoll :: forall e a. Number -> EffKefir e a -> EffKefir e (FromPoll a)

    interval :: forall e a. Number -> a -> EffKefir e (Interval a)

    later :: forall e a. Number -> a -> EffKefir e (Later a)

    map :: forall e stream a b. (StreamLike stream) => (a -> b) -> stream a -> EffKefir e (Stream b)

    mapEff :: forall e stream a b. (StreamLike stream) => (a -> EffKefir e b) -> stream a -> EffKefir e (Stream b)

    never :: forall e. EffKefir e (Never Unit)

    off :: forall e. FunKey -> EffKefir e Unit

    offLog :: forall e stream. (StreamLike stream) => stream _ -> EffKefir e Unit

    onEnd :: forall e stream a. (Terminable stream) => stream a -> EffKefir e _ -> EffKefir e FunKey

    onLog :: forall e stream. (StreamLike stream) => stream _ -> String -> EffKefir e Unit

    onValue :: forall e stream a. (Observable stream) => stream a -> (a -> EffKefir e _) -> EffKefir e FunKey

    repeatedly :: forall e a. Number -> [a] -> EffKefir e (Repeatedly a)

    sequentially :: forall e a. Number -> [a] -> EffKefir e (Sequentially a)

    take :: forall e stream a. (StreamLike stream) => Number -> stream a -> EffKefir e (Stream a)

    takeWhile :: forall e stream a. (StreamLike stream) => (a -> Boolean) -> stream a -> EffKefir e (Stream a)

    takeWhileEff :: forall e stream a. (StreamLike stream) => (a -> EffKefir e Boolean) -> stream a -> EffKefir e (Stream a)

    toProperty :: forall e stream a. (IsStream stream) => stream a -> EffKefir e (Property a)

    toPropertyWith :: forall e stream a. (IsStream stream) => stream a -> a -> EffKefir e (Property a)

    withDefault :: forall e stream a. (StreamLike stream) => stream a -> a -> EffKefir e (Property a)

    withInterval :: forall e a. Number -> (Emitter a -> EffKefir e Unit) -> EffKefir e (WithInterval a)



