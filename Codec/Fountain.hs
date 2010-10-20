-- |  Fountain codes are forward error correction codes for erasure channels.
--    They are able to recover lost packets without needed a backchannel.
--    As a rateless code, transmitters generate packets at random, on the fly.
--    Receivers then listen to as many packets as needed to reconstruct the message.
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
import Data.Word
import System.Random

-- | A decoder holds the state of the decoding process.
data Decoder a = Decoder Int (IntMap a) [Droplet a]

-- | A message droplet is a set of message indices and the combined symbol.
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
decode decoder droplet = decode' decoder [droplet]

decode' :: Bits a => Decoder a -> [Droplet a] -> Either (Decoder a) [a]
decode' (Decoder messageLength symbols _) _ | M.size symbols == messageLength = Right $ map (symbols M.!) [0 .. messageLength - 1]
decode' decoder [] = Left decoder
decode' decoder@(Decoder messageLength symbols droplets) (droplet' : newDroplets)
  | S.size indices == 0 = decode' decoder newDroplets
  | S.size indices == 1 = decode' (Decoder messageLength (M.insert (head $ S.toList indices) symbol symbols) old) (new ++ newDroplets)
  | otherwise           = decode' (Decoder messageLength symbols $ old)                                           (new ++ newDroplets)
  where
  droplet@(Droplet indices symbol) = refineDroplet symbols droplet'
  (new, old) = refineDroplets droplet droplets

refineDroplets :: Bits a => Droplet a -> [Droplet a] -> ([Droplet a], [Droplet a])
refineDroplets d@(Droplet indices symbol) droplets = foldl f ([], [d]) droplets
  where
  --f :: Bits a => ([Droplet a], [Droplet a]) -> Droplet a -> ([Droplet a], [Droplet a])
  f (new, old) d@(Droplet indices1 symbol1)
    | S.isSubsetOf indices indices1 = (Droplet (S.difference indices1 indices) symbol2 : new, old)
    | S.isSubsetOf indices1 indices = (Droplet (S.difference indices indices1) symbol2 : new, d : old)
    | otherwise = (new, d : old)
    where
    symbol2 = symbol `xor` symbol1

refineDroplet :: Bits a => IntMap a -> Droplet a -> Droplet a
refineDroplet symbols (Droplet indices symbol) = f (S.toList indices) S.empty symbol 
  where
  f [] indices symbol = Droplet indices symbol
  f (a : b) indices symbol
    | M.member a symbols = f b indices $ symbol `xor` (symbols M.! a)
    | otherwise          = f b (S.insert a indices) symbol


-- | Runs a test of a Word8 message given a seed, message length, and droplet max degree.
--   Returns the number of droplets that were needed to decode the message.
test :: Int -> Int -> Int -> IO Int
test seed messageLength maxDegree = f 1 (decoder messageLength) (droplets seed' maxDegree message)
  where
  g = mkStdGen seed
  r = randoms g
  message = map fromIntegral $ take messageLength r :: [Word8]
  seed' = r !! messageLength
  f i decoder newDroplets = do
    case decode decoder $ head newDroplets of
      Left decoder -> f (i + 1) decoder $ tail newDroplets
      Right m      -> if m == message then return i else error "failed to reconstruct message"

