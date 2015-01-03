module Test.Modify(main, test) where

import Control.Monad.Eff.Ref

import Data.Either(either)
import Data.Foldable(sum)
import Data.Traversable(for)
import Data.Array(range)
import Data.Foreign.Class(readJSON)

import Test.Assert.Simple
import Test.PSpec hiding (skip)
import Test.PSpec.Mocha

import FRP.Kefir

main = runMocha test

test = do
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
        p <- toPropertyWith "foo" e

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
        w <- withDefault "foo" e

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
        w <- withDefault "foo" p

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
        p <- toPropertyWith "foo" e
        w <- withDefault "xxx" p

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
    
    describe "diff1" $
      itAsync "should calc diffs" $ \done -> do
        p <- sequentially 1 (range 0 10)
        d <- diff1 (\p n -> n - p) p

        onValue d $ \v -> v @?= 1
        onEnd d $ itIs done

    describe "diff" $
      itAsync "should calc diffs" $ \done -> do
        p <- sequentially 1 (range 0 10)
        d <- diff (\p n -> n - p) (-1) p

        onValue d $ \v -> v @?= 1
        onEnd d $ itIs done

    describe "scan1" $
      itAsync "should scan" $ \done -> do
        s <- sequentially 1 (range 3 6)
        n <- scan1 (+) s

        r <- newRef 0
        onValue n $ modifyRef r <<< (+)

        onEnd n $ do
          v <- readRef r
          v @?= 3 + (3 + 4) + (3 + 4 + 5) + (3 + 4 + 5 + 6)
          itIs done
    
    describe "scan" $
      itAsync "should scan with initial value" $ \done -> do
        s <- sequentially 1 (show <$> range 3 6)
        n <- scan (\n i -> n + either (show >>> itIsNot' done) id (readJSON i)) 2 s

        r <- newRef 0
        onValue n $ modifyRef r <<< (+)

        onEnd n $ do
          v <- readRef r
          v @?= 2 + (2 + 3) + (2 + 3 + 4) + (2 + 3 + 4 + 5) + (2 + 3 + 4 + 5 + 6)
          itIs done

    describe "reduce1" $
      itAsync "should reduce to value" $ \done -> do
        s <- sequentially 1 (range 0 10)
        r <- reduce1 (+) s

        onValue r $ (@=?) 55
        onEnd r $ itIs done

    describe "reduce" $
      itAsync "should read and reduce to value" $ \done -> do
        s <- sequentially 1 (show <$> range 0 10)
        r <- reduce (\b a -> either (show >>> itIsNot' done) id (readJSON a) + b) 0 s

        onValue r $ (@=?) 55
        onEnd r $ itIs done

    describe "reduceEff1" $
      itAsync "should reduce to value with side effect" $ \done -> do
        s <- sequentially 1 (range 0 10)
        r <- reduceEff1 (\a b -> return $ a + b) s

        onValue r $ (@=?) 55
        onEnd r $ itIs done

    describe "reduceEff" $
      itAsync "should read and reduce to value with side effect" $ \done -> do
        s <- sequentially 1 (show <$> range 0 10)
        r <- reduceEff (\b a -> either (show >>> itIsNot done) ((+) b >>> return) (readJSON a)) 0 s

        onValue r $ (@=?) 55
        onEnd r $ itIs done

    describe "mapEnd" $
      itAsync "should mapping to End" $ \done -> do
        n <- never
        v <- mapEnd (return "foo") n
        onValue v $ (@=?) "foo"
        onEnd v $ itIs done

    describe "skipEnd" $
      it "should ignore End" $ do
        e <- emitter
        s <- skipEnd e
        
        r <- newRef ""
        onValue s $ writeRef r

        emit e "foo"
        end e
        v <- readRef r
        v @?= "foo"

    describe "slidingWindow" $
      itAsync "should get sliding" $ \done -> do
        s <- sequentially 1 (range 0 5)
        w <- slidingWindow 2 3 s
        r <- reduce (\l i -> i : l) [] w

        onValue r $ \v ->
          v @?= [[3,4,5],[2,3,4],[1,2,3],[0,1,2],[0,1]]

        onEnd r $ itIs done
