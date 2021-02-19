{-# LANGUAGE DeriveFunctor #-}

import qualified Data.Bitstream  as BS
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.Map        as M
import qualified Data.PQueue.Min as P
import           Data.Tuple      (swap)
import           Data.Word       (Word8)
import Data.Function (on)

data Node a
  = Leaf Double a
  | Node Double (Node a) (Node a)
  deriving Show


nodeWeight (Leaf w _  ) = w
nodeWeight (Node w _ _) = w

instance Ord (Node a) where
  compare = compare `on` nodeWeight

instance Eq (Node a) where
  (==) = (==) `on` nodeWeight

instance Semigroup (Node a) where
  l1 <> l2
    | w1 /= 0 && w2 /= 0 = Node (w1 + w2) l1 l2
    | w1 == 0 && w2 /= 0 = l2
    | otherwise          = l1
    where w1 = nodeWeight l1
          w2 = nodeWeight l2

buildHuffmanTree :: Ord a => [(Double, a)] -> Node a
buildHuffmanTree []
  = error "At least one (weight, symbol) tuple must be provided"
buildHuffmanTree l = loop . foldr P.insert P.empty . map (uncurry Leaf) $ l
  where loop q = case P.splitAt 2 q of
                   ([n1, n2], q') -> loop . P.insert (n1 <> n2) $ q'
                   ([n1    ], _ ) -> n1
                   ([]      , _ ) -> error "Empty tree is not possible"
                   _              -> error "Only two elements are taken"

huffmanCodes :: Ord a => Node a -> M.Map a [Bool]
huffmanCodes t = loop M.empty [] t
  where loop m bits (Node _ l r) = ml `M.union` mr
          where ml = loop m (False:bits) l
                mr = loop m (True :bits) r
        loop m bits (Leaf _ a) = M.insert a (reverse bits) m

byteStringCounts :: Num n => ByteString -> [(n, Word8)]
byteStringCounts
  = map swap . M.toList
  . S.foldl' (\m w8 -> M.insertWith (+) w8 1 m) M.empty

data Encoded a
  = Encoded
  { encodedHuffmanTree :: Node Word8
  , encodedPad         :: Int
  , encodedData        :: a
  } deriving (Show, Functor)

encode
  :: ByteString -> Encoded (BS.Bitstream BS.Right)
encode ws
  = Encoded huffmanTree pad bits
  where bits = BS.concat . reverse . bitChunks . S.unpack $ ws
        huffmanTree = buildHuffmanTree . byteStringCounts $ ws
        pad = case (8 - BS.length bits `mod` 8) of
                8 -> 0
                n -> n
        codes = huffmanCodes huffmanTree
        bitCodes = fmap BS.pack codes
        bitChunks
          = maybe (error "An uncounted word encountered") id
          . sequence . fmap (flip M.lookup bitCodes)

encodeBS = fmap BS.toByteString . encode

decodeBools tree = reverse . loop tree
  where loop (Node _ _ r) (True :bs) = loop r bs
        loop (Node _ l _) (False:bs) = loop l bs
        loop (Leaf _ w  ) bs@(_:_)   = w:loop tree bs
        loop (Leaf _ w  ) []         = [w]
        loop _            _          = error "Corrupted data"

decodeBS e = S.pack $ decodeBools (encodedHuffmanTree e) (BS.unpack bsNoPad)
  where str     = encodedData e
        bs      = BS.fromByteString str :: BS.Bitstream BS.Right
        pad     = encodedPad e
        bsNoPad = BS.take (BS.length bs - pad) bs

