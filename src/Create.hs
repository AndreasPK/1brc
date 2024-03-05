{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE BangPatterns #-}
module Create where

import System.Random
import Data.Text.Lazy.Builder as T
import Data.Text.Lazy.Builder as TB
import Data.Text.Lazy.Builder.RealFloat as TB
import Data.Text.Lazy.Builder.Int as TB

import Data.Text.Lazy as T
import qualified Data.Map as M
import Text.Printf
import System.IO
import Data.Text.Lazy.IO as T
import Data.Maybe


-- Make up a list of weater station names, we just use the index number as the name.
names :: M.Map Int Builder
names = M.fromList $ fmap (\x -> (x,(TB.fromString $ (show x ++ ";"))) ) [1..500 :: Int]

-- Make up temps, we just make up random temps between -10/40.
mkValues :: RandomGen t => t -> [(Int, Int)]
mkValues gen0 = go gen0 0
    where
    go _gen 250_000_000 = []
    go gen i =
        -- formatRealFloat is **incredibly** slow, so we make up ints instead between -100 and 400, and then divide by 10 to
        -- get temperatures.
        let (!x,gen') = uniformR (-100,400 ) gen
            (!name,gen'') = uniformR (1,500 :: Int) gen'
        in (name,x):(go gen'' (i+1::Int))

showValue :: (Int,Int) -> Text
showValue (ni,temp) =
    let (!pre,!post) = divMod temp 10
    in
    toLazyText $ (fromJust $ M.lookup ni names) <> TB.decimal pre <> TB.singleton '.' <> TB.decimal post
    -- toLazyText $ (fromJust $ M.lookup ni names) <> TB.formatRealFloat Fixed (Just 1) temp

create :: IO ()
create = do
    f <- openFile "rows.csv" WriteMode
    gen <- getStdGen
    let values = mkValues gen

    mapM_ (\value -> T.hPutStrLn f (showValue value)) values
    hClose f


