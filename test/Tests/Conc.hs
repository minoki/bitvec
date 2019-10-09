module Tests.Conc where

import Control.Concurrent
import Control.Monad
import Data.Bit.ThreadSafe
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as M
import Test.Tasty
import Test.Tasty.HUnit

concTests :: TestTree
concTests = testGroup "Concurrency"
  [ testCase "invertInPlace"  case_conc_invert
  , testCase "reverseInPlace" case_conc_reverse
  ]

case_conc_invert :: IO ()
case_conc_invert = forM_ [0..(1000 :: Int)] $ const $ do
  m <- newEmptyMVar
  let len  = 64
      len' = 37
  vec <- M.replicate len (Bit False)
  let vec1 = M.take len' vec
      vec2 = M.drop len' vec
  _ <- forkIO $ do
    forM_ [0..(1000 :: Int)] $ const $ invertInPlace vec2
    putMVar m ()
  forM_ [0..(1000 :: Int)] $ const $ invertInPlace vec1
  takeMVar m
  wec1 <- V.unsafeFreeze vec1
  wec2 <- V.unsafeFreeze vec2
  let ref1 = V.replicate len' (Bit True)
      ref2 = V.replicate (len - len') (Bit True)
  assertEqual "should be equal" wec1 ref1
  assertEqual "should be equal" wec2 ref2

case_conc_reverse :: IO ()
case_conc_reverse = forM_ [0..(1000 :: Int)] $ const $ do
  m <- newEmptyMVar
  let len  = 256
      len' = 135
  vec <- M.new len
  forM_ [0..len-1] $ \i -> M.write vec i (Bit $ i `rem` 3 == 0)
  let vec1 = M.take len' vec
      vec2 = M.drop len' vec
  _ <- forkIO $ do
    forM_ [0..(1000 :: Int)] $ const $ reverseInPlace vec2
    putMVar m ()
  forM_ [0..(1000 :: Int)] $ const $ reverseInPlace vec1
  takeMVar m
  wec1 <- V.unsafeFreeze vec1
  wec2 <- V.unsafeFreeze vec2
  let ref1 = V.reverse $ V.generate len' (\i -> Bit $ i `rem` 3 == 0)
      ref2 = V.reverse $ V.generate (len - len') (\i -> Bit $ (i + len') `rem` 3 == 0)
  assertEqual "should be equal" wec1 ref1
  assertEqual "should be equal" wec2 ref2
