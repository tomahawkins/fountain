module Codec.Fountain
  ( Droplet (..)
  , Decoder
  , droplets
  , decoder
  , decode
  , test
  ) where

import Data.Bits
import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import Data.IntSet (IntSet)
import qualified Data.IntSet as S
import Data.List
import Data.Word
import System.Random
import Text.Printf

-- | A decoder holds the state of the decoding process.
data Decoder a = Decoder Int (IntMap a) [Droplet a]

-- | A message droplet.
data Droplet a = Droplet IntSet a deriving (Show, Eq)

-- | An infinite list of droplets, given a seed, the max degree, and a message.
--   Only the 'xor' method of 'Bits' is used.
droplets :: Bits a => Int -> Int -> [a] -> [Droplet a]
droplets seed maxDegree message = droplets' $ randoms $ mkStdGen seed
  where
  droplets' a = Droplet indices symbol : droplets' (drop (n + 1) a)
    where
    n = head a `mod` maxDegree + 1
    indices = S.fromList $ map (`mod` length message)  (take n $ tail a)
    symbol = foldl1 xor $ map (message !!) $ S.toList indices

-- | Creates a new decoder given a message length.
decoder :: Int -> Decoder a
decoder messageLength = Decoder messageLength M.empty []

-- | Given a 'Decoder' and a new 'Droplet', returns either an updated 'Decoder' or the decoded message.
--   Only the 'xor' method of 'Bits' is used.
decode :: Bits a => Decoder a -> Droplet a -> Either (Decoder a) [a]
decode (Decoder messageLength knownSymbols droplets) droplet@(Droplet indices symbol)
  | S.size indices == 1 && M.size knownSymbols' == messageLength = Right result
  | S.size indices == 1                                          = Left data1
  | otherwise                                                    = Left data0
  where
  data0 = Decoder messageLength knownSymbols $ droplet : droplets
  data1@(Decoder _ knownSymbols' _) = refine data0
  result = map (knownSymbols' M.!) [0 .. messageLength - 1]

refine :: Bits a => Decoder a -> Decoder a
refine a@(Decoder messageLength knownSymbols droplets) = case getOne droplets of
  Just (Droplet indices symbol, droplets) -> refine $ Decoder messageLength (M.insert i symbol knownSymbols) [ Droplet (S.delete i indices) (if S.member i indices then symbol `xor` symbol' else symbol') | Droplet indices symbol' <- droplets ]
    where
    i = head $ S.toList indices
  Nothing -> a

getOne :: [Droplet a] -> Maybe (Droplet a, [Droplet a])
getOne [] = Nothing
getOne (a@(Droplet indices _) : b)
  | S.size indices == 1 = Just (a, b)
  | otherwise = do
    (i, b) <- getOne b
    return (i, a : b)

-- | Runs a test of a Word8 message given a seed, message length, and droplet max degree.
test :: Int -> Int -> Int -> IO ()
test seed messageLength maxDegree = f 1 (decoder messageLength) (droplets seed' maxDegree message)
  where
  g = mkStdGen seed
  r = randoms g
  message = map fromIntegral $ take messageLength r :: [Word8]
  seed' = r !! messageLength
  f i decoder@(Decoder _ _ droplets) newDroplets = do
    printf "iteration = %4d  droplets = %4d  2d = %4d  3d = %4d  new droplet = %s\n" (i :: Int) (length droplets) (dn 2) (dn 3) (show $ head newDroplets)
    case decode decoder $ head newDroplets of
      Left decoder -> f (i + 1) decoder $ tail newDroplets
      Right m      -> if m == message then return () else error "failed to reconstruct message"
    where
    dropletIndices = [ s | (Droplet s _) <- droplets ]
    dn n = length [ s | s <- dropletIndices, S.size s == n ]

