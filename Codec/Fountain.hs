-- |
-- Module      : Codec.Fountain
-- Copyright   : Tom Hawkins
-- License     : BSD3
--
-- Maintainer  : tomahawkins@gmail.com
-- Portability : unknown
--
-- Fountain codes are forward error correction codes for erasure channels.
-- They are able to recover lost packets without needing a backchannel.
-- As a rateless code, transmitters generate packets at random, on the fly.
-- Receivers then listen to as many packets as needed to reconstruct the message.

module Codec.Fountain
  ( 
  -- * Datatypes
    Droplet (..)
  , Decoder
  , Precoding
  -- * Functions
  , precoding
  , droplets
  , decoder
  , decode
  -- * Test
  , test
  , test'
  , decoderProgress
  ) where

import Data.Bits
import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import Data.IntSet (IntSet)
import qualified Data.IntSet as S
import Data.List
import Data.Word
import System.Random

-- | A decoder holds the state of the decoding process.
data Decoder a = Decoder Int Int (IntMap a) [Droplet a]

-- | A message droplet is a set of message indices and the combined symbol.
data Droplet a = Droplet IntSet a deriving (Show, Eq)

-- | A precoding matrix that appends extra symbols to a message.
type Precoding = [IntSet]

-- | Generates a random precoding matrix.
-- > precoding seed messageLength extraSymbols boundaries
precoding :: Int -> Int -> Int -> (Int, Int) -> Precoding
precoding seed messageLength extraSymbols boundaries = f 0 $ mkStdGen seed
  where
  f :: RandomGen g => Int -> g -> Precoding
  f stage g
    | stage >= extraSymbols = []
    | otherwise = indices : f (stage + 1) g1
    where
    (indices, g1) = randomRow (messageLength + stage) boundaries g

-- Generates a random row of indices, given width and min and max degree.
randomRow :: RandomGen g => Int -> (Int, Int) -> g -> (IntSet, g)
randomRow rowWidth boundaries g = f S.empty g1
  where
  (degree, g1) = randomR boundaries g
  f s g
    | S.size s == degree = (s, g)
    | S.member index s = f s g1
    | otherwise = f (S.insert index s) g1
    where
    (index, g1) = randomR (0, rowWidth - 1) g

-- | An infinite list of droplets, given a seed, the max degree, precoding, and a message.
droplets :: Bits a => Int -> Int -> Precoding -> [a] -> [Droplet a]
droplets seed maxDegree precoding message' = droplets $ mkStdGen seed
  where
  symbol s = foldl1' xor . map (message !!) $ S.toList s
  message = message' ++ [ symbol s | s <- precoding ]
  droplets g = Droplet indices (symbol indices) : droplets g1
    where
    (indices, g1) = randomRow (length message) (0, maxDegree) g

-- | Creates a new decoder given a message length and the precoding.
decoder :: Bits a => Int -> Precoding -> Decoder a
decoder messageLength precoding = f [ Droplet (S.insert i s) 0 | (i, s) <- zip [messageLength ..] precoding ] $ Decoder messageLength (length precoding) M.empty []
  where
  f [] decoder = decoder
  f (a : b) decoder = f b $ fst $ decode decoder a

-- | Given a 'Decoder' and a new 'Droplet', returns either an updated 'Decoder' or the decoded message.
decode :: Bits a => Decoder a -> Droplet a -> (Decoder a, Maybe [a])
decode decoder droplet = decode' decoder [droplet]

decode' :: Bits a => Decoder a -> [Droplet a] -> (Decoder a, Maybe [a])
decode' decoder@(Decoder messageLength _ symbols _) _ | sort [ i | i <- M.keys symbols, i < messageLength ] == [0 .. messageLength - 1] = (decoder, Just $ map (symbols M.!) [0 .. messageLength - 1])
decode' decoder [] = (decoder, Nothing)
decode' decoder@(Decoder messageLength extraSymbols symbols droplets) (droplet' : newDroplets)
  | S.size indices == 0 = decode' decoder newDroplets
  | S.size indices == 1 = decode' (Decoder messageLength extraSymbols (M.insert (head $ S.toList indices) symbol symbols) old) (new ++ newDroplets)
  | otherwise           = decode' (Decoder messageLength extraSymbols symbols $ old)                                           (new ++ newDroplets)
  where
  droplet@(Droplet indices symbol) = refineDroplet symbols droplet'
  (new, old) = refineDroplets droplet droplets

refineDroplets :: Bits a => Droplet a -> [Droplet a] -> ([Droplet a], [Droplet a])
refineDroplets d@(Droplet indices symbol) droplets = foldl f ([], [d]) droplets
  where
  --f :: Bits a => ([Droplet a], [Droplet a]) -> Droplet a -> ([Droplet a], [Droplet a])
  f (new, old) d@(Droplet indices1 symbol1)
    | S.isSubsetOf indices indices1 = (Droplet (S.difference indices1 indices) symbol2 : new, old)
    --  S.isSubsetOf indices1 indices = (Droplet (S.difference indices indices1) symbol2 : new, d : old)
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

-- | Runs a test of a [Word8] message given the message length, max droplet degree, and a seed.
--   Returns the number of droplets that were needed to decode the message and if the message
--   was sucessfully decoded.
test :: Int -> Int -> Precoding -> Int -> (Int, Bool, [Decoder Word8])
test messageLength maxDegree precoding seed = f 1 (decoder messageLength precoding) [] (droplets seed' maxDegree precoding message)
  where
  g = mkStdGen seed
  r = randoms g
  message = map fromIntegral $ take messageLength r :: [Word8]
  seed' = r !! messageLength
  f i decoder decoders newDroplets = case decode decoder $ head newDroplets of
    (decoder', Nothing) -> f (i + 1) decoder' (decoders ++ [decoder]) (tail newDroplets)
    (decoder', Just m)  -> (i, m == message, decoders ++ [decoder, decoder'])

-- | Runs a test with a randomly generated precoding.
-- > test' messageLength dropletMaxDegree extraSymbols (precodingMinDegree, precodingMaxDegree) seed
test' messageLength maxDegree extraSymbols minMaxDegree seed =
  test messageLength maxDegree (precoding (seed + 1) messageLength extraSymbols minMaxDegree) seed

-- | A visual of 'Decoder' progress.
decoderProgress :: Decoder a -> String
decoderProgress (Decoder messageLength extraSymbols m _) = "[" ++
  [ if M.member i m then 'x' else '-' | i <- [0 .. messageLength - 1] ] ++ "|" ++
  [ if M.member i m then 'x' else '-' | i <- [messageLength .. messageLength + extraSymbols - 1] ] ++ "]"

