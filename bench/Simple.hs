{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Criterion.Main
import Servant
import Servant.Criterion

type SampleAPI
    = "test" :> QueryParam "param" Int :> Get '[JSON] String
 :<|> "example" :> Capture ":id" Int :> Delete '[JSON] NoContent

sampleServer :: Server SampleAPI
sampleServer = (return . show) :<|> (\_ -> return NoContent)

sampleInputs :: Inputs (Server SampleAPI)
sampleInputs
    = (Just 123 :-> NoInput)
 :<|> (98723 :-> NoInput)

-- | Benchmark servant-server handlers automatically with servant-criterion.
--
-- Sample output:
--
-- @
-- Running 1 benchmarks...
-- Benchmark simple: RUNNING...
-- benchmarking test?param=123
-- time                 28.83 ns   (28.15 ns .. 29.77 ns)
--                      0.991 R²   (0.986 R² .. 0.995 R²)
-- mean                 30.24 ns   (29.33 ns .. 31.28 ns)
-- std dev              3.190 ns   (2.597 ns .. 3.900 ns)
-- variance introduced by outliers: 92% (severely inflated)
--
-- benchmarking example/98723
-- time                 441.6 ps   (429.7 ps .. 452.6 ps)
--                      0.994 R²   (0.992 R² .. 0.996 R²)
-- mean                 444.6 ps   (432.6 ps .. 460.9 ps)
-- std dev              44.46 ps   (35.03 ps .. 62.24 ps)
-- variance introduced by outliers: 93% (severely inflated)
--
-- Benchmark simple: FINISH
-- @
main :: IO ()
main = defaultMain $ benchHandlers (Proxy @SampleAPI) sampleServer sampleInputs
