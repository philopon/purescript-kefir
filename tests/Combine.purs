module Test.Combine(main, test) where

import Control.Monad.Eff.Ref

import Test.Assert.Simple
import Test.PSpec hiding (skip)
import Test.PSpec.Mocha

import FRP.Kefir

main = runMocha test

test = do
  describe "combine" $
    itAsync "combine 2 input" $ \done -> do
      foo <- emitter
      bar <- emitter
      cmb <- combine foo bar $ \f b -> f + b

      ref <- newRef []
      onValue cmb $ modifyRef ref <<< (:)
      onEnd cmb $ do
        v <- readRef ref
        v @?= [5,3]
        itIs done

      emit foo 1
      emit bar 2
      emit foo 3
      end foo
      end bar

  describe "and" $
    itAsync "combine 3 boolean input using &&" $ \done -> do
      a <- emitter
      b <- emitter
      c <- emitter
      at <- and [a,b,c]

      ref <- newRef []

      onValue at $ modifyRef ref <<< (:)
      onEnd at $ do
        v <- readRef ref
        v @?= [false, true, false]
        itIs done

      emit a true
      emit b false
      emit c true
      emit b true
      emit a false

      end a
      end b
      end c

  describe "or" $
    itAsync "combine 3 boolean input using ||" $ \done -> do
      a <- emitter
      b <- emitter
      c <- emitter
      at <- or [a,b,c]

      ref <- newRef []

      onValue at $ modifyRef ref <<< (:)
      onEnd at $ do
        v <- readRef ref
        v @?= [true,true,true]
        itIs done

      emit a true
      emit b false
      emit c true
      emit b true
      emit a false

      end a
      end b
      end c

  describe "sampledBy" $
    itAsync "should sample passive by active" $ \done -> do
      pas <- repeatedly   20 [5,6,7]
      act <- sequentially 20 [0,1,2] >>= delay 10

      sam <- sampledBy pas act (+)
      ref <- newRef []
      onValue sam $ modifyRef ref <<< (:)
      onEnd sam $ do
        v <- readRef ref
        v @?= [9,7,5]
        itIs done

  describe "zipWith" $
    itAsync "should zip with plus" $ \done -> do
      a <- sequentially 20 [0,1,2,3]
      b <- sequentially 32 [4,5,6]

      zip <- zipWith (+) a b
      ref <- newRef []
      onValue zip $ modifyRef ref <<< (:)
      onEnd zip $ do
        v <- readRef ref
        v @?= [8,6,4]
        itIs done

  describe "merge" $
    itAsync "should merge streams" $ \done -> do
      a <- sequentially 10 [0,2,4]
      b <- sequentially 10 [1,3,5] >>= delay 5
      m <- merge [forget a,b]

      ref <- newRef []
      onValue m $ modifyRef ref <<< (:)
      onEnd m $ do
        v <- readRef ref
        v @?= [5,4,3,2,1,0]
        itIs done

  describe "concat" $
    itAsync "concat 3 emitters" $ \done -> do
      a <- emitter
      b <- emitter
      c <- emitter
      abc <- concat [a,b,c]

      ref <- newRef []
      onValue abc $ modifyRef ref <<< (:)
      onEnd abc $ do
        v <- readRef ref
        v @?= [2,2,1,2,1,0]
        itIs done

      emit a 0
      emit a 1
      emit b 0
      emit a 2
      end a
      emit c 0
      emit b 1
      emit c 1
      emit b 2
      end b
      emit c 2
      end c

  describe "pool" $
    it "should merge dynamically" $ do
      a <- emitter
      b <- emitter

      pool <- pool
      ref  <- newRef []
      onValue pool $ modifyRef ref <<< (:)

      emit a 1
      emit b 1
      plug pool a
      emit a 2
      emit b 2
      plug pool b
      emit a 3
      emit b 3
      unPlug pool a
      emit a 4
      emit b 4
      end a
      end b

      v <- readRef ref
      v @?= [4,3,3,2]

  describe "bus" $
    itAsync "should merge dynamically with emittable" $ \done -> do
      b <- bus
      e <- emitter
      plug b e

      ref <- newRef []
      onValue b $ modifyRef ref <<< (:)
      onEnd b $ do
        v <- readRef ref
        v @?= [2,1]
        itIs done

      emit b 1
      emit e 2
      end  b

  describe "flatMap" $
    itAsync "should flatMap" $ \done -> do
      a <- interval 20 1 >>= take 4
      b <- interval 20 2 >>= take 4
      c <- interval 20 3 >>= take 4
      iv <- sequentially 50 [a,b,c]
      fm <- flatMap iv

      ref <- newRef []
      onValue fm $ modifyRef ref <<< (:)
      onEnd fm $ do
        v <- readRef ref
        v @?= [3,3,3,2,3,2,2,1,2,1,1,1]
        itIs done

  describe "flatMapWith" $
    itAsync "should flatMapWith" $ \done -> do
      sq <- sequentially 50 [1,2,3]
      fm <- flatMapWith sq $ \i ->
        interval 20 i >>= take 4

      ref <- newRef []
      onValue fm $ modifyRef ref <<< (:)
      onEnd fm $ do
        v <- readRef ref
        v @?= [3,3,3,2,3,2,2,1,2,1,1,1]
        itIs done

  describe "flatMapLatest" $
    itAsync "should flatMapLatest" $ \done -> do
      a <- interval 20 1 >>= take 4
      b <- interval 20 2 >>= take 4
      c <- interval 20 3 >>= take 4
      iv <- sequentially 50 [a,b,c]
      fm <- flatMapLatest iv

      ref <- newRef []
      onValue fm $ modifyRef ref <<< (:)
      onEnd fm $ do
        v <- readRef ref
        v @?= [3,3,3,3,2,2,1,1]
        itIs done

  describe "flatMapLatestWith" $
    itAsync "should flatMapLatestWith" $ \done -> do
      sq <- sequentially 50 [1,2,3]
      fm <- flatMapLatestWith sq $ \i ->
        interval 20 i >>= take 4

      ref <- newRef []
      onValue fm $ modifyRef ref <<< (:)
      onEnd fm $ do
        v <- readRef ref
        v @?= [3,3,3,3,2,2,1,1]
        itIs done

  describe "flatMapFirst" $
    itAsync "should flatMapFirst" $ \done -> do
      a <- interval 20 1 >>= take 4
      b <- interval 20 2 >>= take 4
      c <- interval 20 3 >>= take 4
      iv <- sequentially 50 [a,b,c]
      fm <- flatMapFirst iv

      ref <- newRef []
      onValue fm $ modifyRef ref <<< (:)
      onEnd fm $ do
        v <- readRef ref
        v @?= [3,3,3,3,1,1,1,1]
        itIs done

  describe "flatMapFirstWith" $
    itAsync "should flatMapFirstWith" $ \done -> do
      sq <- sequentially 50 [1,2,3]
      fm <- flatMapFirstWith sq $ \i ->
        interval 20 i >>= take 4

      ref <- newRef []
      onValue fm $ modifyRef ref <<< (:)
      onEnd fm $ do
        v <- readRef ref
        v @?= [3,3,3,3,1,1,1,1]
        itIs done

  describe "flatMapConcat" $
    itAsync "should flatMapConcat" $ \done -> do
      a <- interval 20 1 >>= take 4
      b <- interval 20 2 >>= take 4
      c <- interval 20 3 >>= take 4
      iv <- sequentially 50 [a,b,c]
      fm <- flatMapConcat iv

      ref <- newRef []
      onValue fm $ modifyRef ref <<< (:)
      onEnd fm $ do
        v <- readRef ref
        v @?= [3,3,3,3,2,2,2,2,1,1,1,1]
        itIs done

  describe "flatMapConcatWith" $
    itAsync "should flatMapConcatWith" $ \done -> do
      sq <- sequentially 50 [1,2,3]
      fm <- flatMapConcatWith sq $ \i ->
        interval 20 i >>= take 4

      ref <- newRef []
      onValue fm $ modifyRef ref <<< (:)
      onEnd fm $ do
        v <- readRef ref
        v @?= [3,3,3,3,2,2,2,2,1,1,1,1]
        itIs done

  describe "flatMapConcurLimit" $
    itAsync "should flatMapConcurLimit" $ \done -> do
      a <- interval 20 1 >>= take 6
      b <- interval 20 2 >>= take 6
      c <- interval 20 3 >>= take 6
      iv <- sequentially 50 [a,b,c]
      fm <- flatMapConcurLimit 2 iv

      ref <- newRef []
      onValue fm $ modifyRef ref <<< (:)
      onEnd fm $ do
        v <- readRef ref
        v @?= [3,3,3,3,2,3,2,3,2,1,2,1,2,1,2,1,1,1]
        itIs done

  describe "flatMapConcurLimit" $
    itAsync "should flatMapConcurLimit" $ \done -> do
      sq <- sequentially 50 [1,2,3]
      fm <- flatMapConcurLimitWith 2 sq $ \i ->
        interval 20 i >>= take 6

      ref <- newRef []
      onValue fm $ modifyRef ref <<< (:)
      onEnd fm $ do
        v <- readRef ref
        v @?= [3,3,3,3,2,3,2,3,2,1,2,1,2,1,2,1,1,1]
        itIs done
