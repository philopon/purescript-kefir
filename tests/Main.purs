module Test.Main where

import Control.Monad.Eff.Ref
import Data.Traversable
import Data.Foldable
import qualified Control.Timer as T
import Data.Date
import Data.Array(range)

import Test.Assert.Simple
import Test.PSpec hiding (skip)
import Test.PSpec.Mocha

import FRP.Kefir

assertAbout e a = if e * 0.9 < a && a < e * 1.1 then return unit else assertFailure msg
  where
    msg = "expected: about " ++ show e ++ " but got: " ++ show a

main = runMocha $ do
  describe "stream" $ do
    describe "emitter" $ do
      itAsync "should take value" $ \done -> do
        e <- emitter
        onValue e $ \v -> do
          v @?= "foo"
          itIs done

        onEnd e $ itIsNot done "end"
        emit e "foo"

      itAsync "should take end" $ \done -> do
        e <- emitter
        onValue e $ \_ -> do
          itIsNot done "value"
        onEnd e $ itIs done
        end e

    describe "later" $ do
      itAsync "should emit 1 value and end, 100ms later." $ \done -> do
        st <- now
        l  <- later 50 "bar"
        onValue l $ \v -> do
          v @?= "bar"
        onEnd l $ do
          ed <- now
          assertAbout 50 (toEpochMilliseconds ed - toEpochMilliseconds st)
          itIs done

    describe "interval" $ do
      itAsync "should emit same values, forever." $ \done -> do
        i <- interval 50 "baz"
        r <- newRef ""
        k <- onValue i $ \v -> modifyRef r (\a -> a ++ v)
        T.timeout 175 $ do
          v <- readRef r
          v @?= "bazbazbaz"
          off k
          itIs done

    describe "sequentially" $ do
      itAsync "should emit values, and end" $ \done -> do
        s <- sequentially 50 ["foo", "bar", "baz"]
        r <- newRef ""
        onValue s $ \v -> modifyRef r (\a -> a ++ v)
        onEnd s $ do
          v <- readRef r
          v @?= "foobarbaz"
          itIs done

    describe "repeatedly" $ do
      itAsync "should emit values forever" $ \done -> do
        p <- repeatedly 50 ["foo", "bar"]
        r <- newRef ""
        k <- onValue p $ \v -> modifyRef r (\a -> a ++ v)
        T.timeout 175 $ do
          v <- readRef r
          v @?= "foobarfoo"
          off k
          itIs done

    describe "fromPoll" $ do
      itAsync "should polling" $ \done -> do
        c <- newRef 7
        p <- fromPoll 50 $ modifyRef' c (\i -> {retVal: i, newState: i * 2})
        r <- newRef 0
        k <- onValue p $ \v -> modifyRef r (\a -> a + v)
        T.timeout 125 $ do
          v <- readRef r
          v @?= 7 * 3
          off k
          itIs done

    describe "withInterval" $
      itAsync "should send 3 msg and end." $ \done -> do
        c <- newRef 0
        i <- withInterval 50 $ \em -> do
          n <- modifyRef' c $ \i -> {retVal: i, newState: i + 1}
          if n < 3 then emit em "foo" else end em

        r <- newRef ""
        onValue i $ \v -> modifyRef r (\a -> a ++ v)
        onEnd i $ do
          v <- readRef r
          v @?= "foofoofoo"
          itIs done

    describe "fromCallback" $
      itAsync "sould send once and end." $ \done -> do
        f <- fromCallback $ \cb -> cb "bar"
        r <- newRef ""
        onValue f $ \v -> modifyRef r (\a -> a ++ v)
        onEnd f $ do
          v <- readRef r
          v @?= "bar"
          itIs done

    describe "fromBinder" $
      itAsync "" $ \done -> do
        r <- newRef ""

        b <- fromBinder $ \e -> do
          emit e "emit"
          T.timeout 50 $ end e
          return $ do
            modifyRef r (\a -> a ++ "end")

        onValue b $ \v -> modifyRef r (\a -> a ++ v)
        onEnd b $ T.timeout 50 $ do
          v <- readRef r
          v @?= "emitend"
          itIs done

  describe "property" $
    describe "constant" $
      itAsync "must send constant and end" $ \done -> do
        c <- constant "foo"

        r <- newRef ""
        onValue c $ \v -> modifyRef r ((++) v)
        onEnd c $ do
          v <- readRef r
          v @?= "foo"
          itIs done

  describe "convert" $ do
    describe "toProperty" $
      itAsync "should be same to original." $ \done -> do
        e <- emitter
        p <- toProperty e

        onValue p $ \v -> v @?= "emit"
        onEnd p $ itIs done

        emit e "emit"
        end e

    describe "toPropertyWith" $
      itAsync "should send current and original." $ \done -> do
        e <- emitter
        p <- toPropertyWith e "foo"

        r <- newRef ""
        onValue p $ \v -> modifyRef r (\a -> a ++ v)
        onEnd p $ do
          v <- readRef r
          v @?= "foobar"
          itIs done

        emit e "bar"
        end e

    describe "changes" $
      itAsync "should be same to original." $ \done -> do
        c <- constant "foo"
        s <- changes c

        onValue s $ \v -> v @?= "foo"
        onEnd s $ itIs done

    describe "withDefault" $ do
      itAsync "should add default to stream" $ \done -> do
        e <- emitter
        w <- withDefault e "foo"

        r <- newRef ""
        onValue w $ \v -> modifyRef r (\a -> a ++ v)
        onEnd w $ do
          v <- readRef r
          v @?= "foobar"
          itIs done

        emit e "bar"
        end e

      itAsync "should add default to property missing current value" $ \done -> do
        e <- emitter
        p <- toProperty e
        w <- withDefault p "foo"

        r <- newRef ""
        onValue w $ \v -> modifyRef r (\a -> a ++ v)
        onEnd w $ do
          v <- readRef r
          v @?= "foobar"
          itIs done

        emit e "bar"
        end e

      itAsync "should be id when property has current value" $ \done -> do
        e <- emitter
        p <- toPropertyWith e "foo"
        w <- withDefault p "xxx"

        r <- newRef ""
        onValue w $ \v -> modifyRef r (\a -> a ++ v)
        onEnd w $ do
          v <- readRef r
          v @?= "foobar"
          itIs done

        emit e "bar"
        end e

  describe "modify an observable" $ do
    describe "map" $
      itAsync "should mapping value" $ \done -> do
        e <- emitter
        f <- map show e

        r <- newRef ""

        onValue f $ modifyRef r <<< flip (++)

        onEnd f $ do
          v <- readRef r
          v @?= "1234567890"
          itIs done

        emit e 123
        emit e 456
        emit e 789
        emit e 0
        end e

    describe "mapEff" $
      itAsync "should mapping value with side effect" $ \done -> do
        e <- emitter

        re <- newRef 0
        f  <- mapEff (\v -> modifyRef re ((+) v) >>= \_ -> return v) e

        rf <- newRef 0
        onValue e $ modifyRef rf <<< (+)

        onEnd f $ do
          ve <- readRef re
          vf <- readRef rf

          ve @?= vf
          itIs done
        
        emit e 35
        emit e (-1)
        end e
        emit e 65

    describe "filter" $
      itAsync "should take even only" $ \done -> do
        e <- emitter
        f <- filter (\v -> v % 2 == 0) e

        onValue f $ \v -> v % 2 @?= 0
        onEnd   f $ itIs done

        for (range 0 100) (emit e)
        end e

    describe "filterEff" $
      itAsync "should take even only with side effect" $ \done -> do
        e <- emitter
        r <- newRef 0
        f <- filterEff (\v -> modifyRef r ((+) v) >>= \_ -> return (v % 2 == 0)) e

        let ary = range 0 100

        onValue f $ \v -> v % 2 @?= 0
        onEnd   f $ do
          v <- readRef r
          v @?= sum ary
          itIs done

        for ary (emit e)
        end e

    describe "take" $
      itAsync "should take 10 values and end" $ \done -> do
        p <- repeatedly 1 (range 0 100)
        f <- take 10 p

        r <- newRef 0
        onValue f $ modifyRef r <<< (+)
        onEnd f $ do
          v <- readRef r
          v @?= sum (range 0 9)
          itIs done

    describe "takeWhile" $
      itAsync "should take while <10 and end" $ \done -> do
        p <- repeatedly 1 (range 0 100)
        f <- takeWhile ((>) 10) p

        r <- newRef 0
        onValue f $ modifyRef r <<< (+)
        onEnd f $ do
          v <- readRef r
          v @?= sum (range 0 9)
          itIs done

    describe "takeWhileEff" $
      itAsync "should take while <10 and end with side effect" $ \done -> do
        p <- repeatedly 1 (range 0 100)
        f <- takeWhileEff (\v -> return $ 10 > v) p

        r <- newRef 0
        onValue f $ modifyRef r <<< (+)
        onEnd f $ do
          v <- readRef r
          v @?= sum (range 0 9)
          itIs done

    describe "skip" $
      itAsync "should skip 10 values" $ \done -> do
        p <- sequentially 1 (range 0 30)
        f <- skip 10 p

        r <- newRef 0
        onValue f $ modifyRef r <<< (+)
        onEnd f $ do
          v <- readRef r
          v @?= sum (range 10 30)
          itIs done

    describe "skipWhile" $
      itAsync "should skip while <10" $ \done -> do
        p <- sequentially 1 (range 0 30)
        f <- skipWhile ((>) 10) p

        r <- newRef 0
        onValue f $ modifyRef r <<< (+)
        onEnd f $ do
          v <- readRef r
          v @?= sum (range 10 30)
          itIs done

    describe "skipWhileEff" $
      itAsync "should skip while <10 with side effect" $ \done -> do
        p <- sequentially 1 (range 0 30)
        f <- skipWhileEff (\v -> return $ 10 > v) p

        r <- newRef 0
        onValue f $ modifyRef r <<< (+)
        onEnd f $ do
          v <- readRef r
          v @?= sum (range 10 30)
          itIs done

    describe "skipDuplicates" $
      itAsync "should skip duplicates" $ \done -> do
        p <- sequentially 1 [1,2,2,3,3,3,4,4,4,4,5,5,5,5,5]
        s <- skipDuplicates p

        r <- newRef []
        onValue s $ \v -> modifyRef r (\l -> v:l)

        onEnd s $ do
          v <- readRef r
          v @?= [5,4,3,2,1]
          itIs done
