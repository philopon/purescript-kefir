module Test.Stream (main, test) where

import Control.Monad.Eff.Ref
import qualified Control.Timer as T

import Data.Date

import Test.Common
import Test.Assert.Simple
import Test.PSpec hiding (skip)
import Test.PSpec.Mocha
import Data.Either

import FRP.Kefir

main = runMocha test

test = do
  describe "stream" $ do
    describe "emitter" $ do
      itAsync "should take value" $ \done -> do
        e <- emitter
        onValue e $ \v -> do
          v @?= "foo"
          itIs done

        onError e $ \e -> itIsNot done "error"

        onEnd e $ itIsNot done "end"
        emitAsync e "foo"

      itAsync "should take end" $ \done -> do
        e <- emitter
        onValue e $ \_ -> itIsNot done "value"
        onError e $ \_ -> itIsNot done "error"
        onEnd e $ itIs done
        endAsync e

      itAsync "should take error" $ \done -> do
        e <- emitter
        onValue e $ \e -> itIsNot done "value"

        onError e $ \v -> do
          v @?= "foo"
          itIs done

        onEnd e $ itIsNot done "end"
        errorAsync e "foo"

    describe "never" $
      itAsync "should send end" $ \done -> do
        n <- never
        onEnd n $ itIs done

    describe "later" $ do
      itAsync "should emit 1 value and end, 50ms later." $ \done -> do
        st <- now
        l  <- later 50 "bar"
        onValue l $ \v -> do
          v @?= "bar"
        onEnd l $ do
          ed <- now
          assertAbout 0.2 50 (toEpochMilliseconds ed - toEpochMilliseconds st)
          itIs done

    describe "interval" $ do
      itAsync "should emit same values, forever." $ \done -> do
        i    <- interval 50 "baz"
        r    <- newRef ""
        offk <- onValue i $ \v -> modifyRef r (\a -> a ++ v)
        T.timeout 175 $ do
          v <- readRef r
          v @?= "bazbazbaz"
          offk
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
        p    <- repeatedly 50 ["foo", "bar"]
        r    <- newRef ""
        offk <- onValue p $ \v -> modifyRef r (\a -> a ++ v)
        T.timeout 175 $ do
          v <- readRef r
          v @?= "foobarfoo"
          offk
          itIs done

    describe "fromPoll" $ do
      itAsync "should polling" $ \done -> do
        c    <- newRef 7
        p    <- fromPoll 50 $ modifyRef' c (\i -> {retVal: i, newState: i * 2})
        r    <- newRef 0
        offk <- onValue p $ \v -> modifyRef r (\a -> a + v)
        T.timeout 125 $ do
          v <- readRef r
          v @?= 7 * 3
          offk
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
        f <- fromCallback $ return "bar"
        r <- newRef ""
        onValue f $ \v -> modifyRef r (\a -> a ++ v)
        onEnd f $ do
          v <- readRef r
          v @?= "bar"
          itIs done

    describe "fromNodeCallback" $ do
      itAsync "sould send one error and end." $ \done -> do
        f <- fromNodeCallback $ return (Left "bar")

        r <- newRef ""
        onError f $ \v -> modifyRef r (\a -> "e:" ++ a ++ v)
        onEnd f $ do
          v <- readRef r
          v @?= "e:bar"
          itIs done

      itAsync "sould send one value and end." $ \done -> do
        f <- fromNodeCallback $ return (Right "bar")

        r <- newRef ""
        onValue f $ \v -> modifyRef r (\a -> "v:" ++ a ++ v)
        onError f $ \_ -> itIsNot done "error"
        onEnd f $ do
          v <- readRef r
          v @?= "v:bar"
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

  describe "property" $ do
    describe "constant" $
      itAsync "must send constant and end" $ \done -> do
        c <- constant "foo"

        r <- newRef ""
        onValue c $ \v -> modifyRef r ((++) v)
        onEnd c $ do
          v <- readRef r
          v @?= "foo"
          itIs done

    describe "constantError" $
      itAsync "must send constant error and end" $ \done -> do
        c <- constantError "foo"

        r <- newRef ""
        onError c $ \v -> modifyRef r ((++) v)
        onEnd c $ do
          v <- readRef r
          v @?= "foo"
          itIs done

  describe "onAny" $ itAsync "should observe any events" $ \done -> do
    emt <- emitter
    prp <- toPropertyWith "def" emt
    ref <- newRef ""
    onAny prp $ \ev -> case ev of
      Value cur v | cur       -> modifyRef ref ((++) $ "v:" ++ v)
                  | otherwise -> modifyRef ref ((++) $ "c:" ++ v)
      Error cur v | cur       -> modifyRef ref ((++) $ "x:" ++ v)
                  | otherwise -> modifyRef ref ((++) $ "e:" ++ v)
      End -> do
        v <- readRef ref
        v @?= "e:errc:foov:def"
        itIs done

    emit emt "foo"
    error emt "err"
    end emt
