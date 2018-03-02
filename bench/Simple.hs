{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Criterion.Main
import Servant
import Servant.Criterion

-- | This is an API we want to benchmark.
type SimpleAPI
    = "test" :> QueryParam "param" Int :> Get '[JSON] String
 :<|> "example" :> Capture ":id" String :> Delete '[JSON] NoContent

-- | These is the server handler implementations for 'SimpleAPI'.
simpleServer :: Server SimpleAPI
simpleServer = (return . show) :<|> (\_ -> return NoContent)

-- | These are the inputs we're going to use for the benchmark below.
simpleInputs :: Inputs (Server SimpleAPI)
simpleInputs
    = (Just 256 :-> NoInput)
 :<|> ("dead-beef" :-> NoInput)

-- | Benchmark servant-server handlers automatically with servant-criterion.
--
-- Sample output:
--
-- @
-- Running 1 benchmarks...
-- Benchmark simple: RUNNING...
-- benchmarking test?param=256
-- time                 30.29 ns   (28.86 ns .. 31.77 ns)
--                      0.988 R²   (0.983 R² .. 0.994 R²)
-- mean                 30.16 ns   (29.33 ns .. 31.28 ns)
-- std dev              3.103 ns   (2.615 ns .. 3.797 ns)
-- variance introduced by outliers: 92% (severely inflated)
--
-- benchmarking example/dead-beef
-- time                 445.1 ps   (433.5 ps .. 457.6 ps)
--                      0.995 R²   (0.993 R² .. 0.997 R²)
-- mean                 458.6 ps   (446.9 ps .. 473.8 ps)
-- std dev              40.96 ps   (31.65 ps .. 51.34 ps)
-- variance introduced by outliers: 92% (severely inflated)
--
-- Benchmark simple: FINISH
-- @
main :: IO ()
main = defaultMain $ benchHandlers (Proxy @SimpleAPI) simpleServer simpleInputs
