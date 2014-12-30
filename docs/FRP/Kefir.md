# Module Documentation

## Module FRP.Kefir

### Types

    data DummyStream :: *

    type EffKefir e = Eff (kefir :: Kefir | e)

    newtype Emitter a where
      Emitter :: Stream a -> Emitter a

    newtype Endless a where
      Endless :: Stream a -> Endless a

    newtype FunKey where
      FunKey :: { function :: RegisteredFunction, target :: Target, stream :: DummyStream } -> FunKey

    data Kefir :: !

    data RegisteredFunction :: *

    data Stream :: * -> *

    newtype Target where
      Target :: Number -> Target

    type Targets = { end :: Target, value :: Target }


### Type Classes

    class Emittable stream where
      emit :: forall e a. stream a -> a -> EffKefir e Unit

    class Endable stream where
      end :: forall e a. stream a -> EffKefir e Unit

    class Limitted stream where
      onEnd :: forall e a b. stream a -> EffKefir e b -> EffKefir e FunKey

    class Observable stream where
      onValue :: forall e a b. stream a -> (a -> EffKefir e b) -> EffKefir e FunKey
      onLog :: forall e a. stream a -> String -> EffKefir e Unit
      offLog :: forall e a. stream a -> EffKefir e Unit


### Type Class Instances

    instance emittableStream :: Emittable Emitter

    instance endableStream :: Endable Emitter

    instance limittedEmitter :: Limitted Emitter

    instance limittedStream :: Limitted Stream

    instance observableEmitter :: Observable Emitter

    instance observableEndless :: Observable Endless

    instance observableStream :: Observable Stream


### Values

    call0Eff :: forall e o r. Fn2 String o (Eff e r)

    call1Eff :: forall e o a r. Fn3 String o a (Eff e r)

    call2Eff :: forall e o a b r. Fn4 String o a b (Eff e r)

    emitter :: forall e a. EffKefir e (Emitter a)

    execute :: forall a. a -> a

    fromCallback :: forall e a. ((a -> EffKefir e Unit) -> EffKefir e Unit) -> EffKefir e (Stream a)

    fromPoll :: forall e a. Number -> EffKefir e a -> EffKefir e (Endless a)

    interval :: forall e a. Number -> a -> EffKefir e (Endless a)

    later :: forall e a. Number -> a -> EffKefir e (Stream a)

    never :: forall e a. EffKefir e (Stream a)

    off :: forall e. FunKey -> EffKefir e Unit

    offImpl :: forall e. Fn2 Targets FunKey (EffKefir e Unit)

    onEndImpl :: forall stream e a b. Fn3 Target (stream a) (EffKefir e b) (EffKefir e FunKey)

    onValueImpl :: forall stream e a b. Fn3 Target (stream a) (a -> EffKefir e b) (EffKefir e FunKey)

    repeatedly :: forall e a. Number -> [a] -> EffKefir e (Endless a)

    sequentially :: forall e a. Number -> [a] -> EffKefir e (Stream a)

    targetEnd :: Target

    targetValue :: Target

    targets :: Targets

    withInterval :: forall e a. Number -> (Emitter a -> EffKefir e Unit) -> EffKefir e (Stream a)



