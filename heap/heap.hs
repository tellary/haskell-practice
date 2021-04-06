{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}


import Control.Monad       (when)
import Control.Monad.Catch (Exception, throwM)
import Data.Array.IO       (IOUArray)
import Data.Array.MArray   (MArray (getBounds), getElems, newListArray,
                            readArray, writeArray)

left i = i*2
right i = i*2 + 1

swap a i j = do
  t <- readArray a i
  writeArray a i =<< readArray a j
  writeArray a j t

data BoundsException = MustStart1 deriving Show

instance Exception BoundsException

heapify a i = do
  lower <- fst <$> getBounds a
  when (lower /= 1) $ throwM MustStart1
  let l = left i
      r = right i
  maxIx <- snd <$> getBounds a
  v <- readArray a i
  (largest1, v1) <- if (l <= maxIx)
                    then do
                      lv <- readArray a l
                      if (lv > v) then return (l, lv) else return (i, v)
                    else return (i, v)
  largest2 <- if (r <= maxIx)
              then do
                lr <- readArray a r
                if (lr > v1) then return r else return largest1
              else return largest1
  if largest2 /= i then do
    swap a i largest2
    heapify a largest2 else return ()

buildHeap a = do
  size <- snd <$> getBounds a
  let halfSize = size `div` 2
  mapM_ (heapify a) [halfSize, halfSize - 1..1]

buildHeapList :: forall a. (MArray IOUArray a IO, Ord a) => [a] -> IO [a]
buildHeapList l = do
  a <- (newListArray (1, length l) l :: IO (IOUArray Int a))
  buildHeap a
  getElems a

t1 = buildHeapList [21,7,8,12,4,16,11] :: IO [Int]

