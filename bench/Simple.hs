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

main :: IO ()
main = defaultMain $ benchHandlers (Proxy @SampleAPI) sampleServer sampleInputs
