{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Servant.Criterion where

import Control.DeepSeq
import Criterion.Main
import Data.Monoid ((<>))
import GHC.Exts (Constraint)
import GHC.Generics (Generic)
import Servant

data T a = L a | B (T a) (T a)

data a :-> b = a :-> b
  deriving (Show)
infixr 4 :->

data NoInput = NoInput
  deriving (Show)

-- | A type for all inputs to a multi-function.
--
-- >>> :kind! Inputs (Int -> (String :<|> (Char -> Bool)))
-- Inputs (Int -> (String :<|> (Char -> Bool))) :: *
-- = Int :-> (NoInput :<|> (Char :-> NoInput))
type family Inputs a where
  Inputs (a :<|> b)  = Inputs a :<|> Inputs b
  Inputs (a -> b)    = a :-> Inputs b
  Inputs a           = NoInput

-- |
-- >>> :kind! ReturnTypes (Int -> (String :<|> (Char -> IO Bool)))
-- ReturnTypes IO (Int -> (String :<|> (Char -> IO Bool))) :: T *
-- = 'B ('L [Char]) ('L (IO Bool))
type family ReturnTypes mf where
  ReturnTypes (a :<|> b)  = 'B (ReturnTypes a) (ReturnTypes b)
  ReturnTypes (a -> b)    = ReturnTypes b
  ReturnTypes a           = 'L a

type family EndpointsT api where
  EndpointsT (a :<|> b) = 'B (EndpointsT a) (EndpointsT b)
  EndpointsT (a :> b) = PrependParam a (EndpointsT b)
  EndpointsT a = 'L a

type family PrependParam param api where
  PrependParam param ('B a b) = 'B (PrependParam param a) (PrependParam param b)
  PrependParam param ('L a) = 'L (param :> a)

type family All c xs :: Constraint where
  All c ('B a b) = (All c a, All c b)
  All c ('L a) = c a

class ApplyInputs api mf where
  applyInputs
    :: (Monoid m, All c (ReturnTypes mf), All ce (EndpointsT api))
    => Proxy (api :: *)
    -> Proxy c
    -> Proxy ce
    -> (forall (endpoint :: *) ret. (c ret, ce endpoint) => Proxy endpoint -> ret -> m)
    -> mf
    -> Inputs mf
    -> m

instance {-# OVERLAPPABLE #-} (EndpointsT endpoint ~ 'L endpoint, Inputs a ~ NoInput, ReturnTypes a ~ 'L a) => ApplyInputs endpoint a where
  applyInputs api _ _ f server NoInput = f api server

instance ApplyInputs endpoint api => ApplyInputs endpoint (param -> api) where
  applyInputs api pc pce f server (a :-> inputs) = applyInputs api pc pce f (server a) inputs

instance (ApplyInputs ea a, ApplyInputs eb b) => ApplyInputs (ea :<|> eb) (a :<|> b) where
  applyInputs _ pc pce f (sa :<|> sb) (a :<|> b)
    = applyInputs (Proxy @ea) pc pce f sa a <> applyInputs (Proxy @eb) pc pce f sb b

class Unconstrained endpoint
instance Unconstrained endpoint

class NFDataHandler h where nfHandler :: h -> Benchmarkable
instance NFData a => NFDataHandler (Handler a) where nfHandler = nfIO . runHandler

benchmarkableHandlers ::
  ( All Unconstrained (EndpointsT api)
  , All NFDataHandler (ReturnTypes server)
  , ApplyInputs api server
  ) => Proxy api -> server -> Inputs server -> [Benchmarkable]
benchmarkableHandlers api = applyInputs api (Proxy @NFDataHandler) (Proxy @Unconstrained) f
  where
    f _endpoint handler = [nfHandler handler]

class (a ~ Link) => IsLink a
instance IsLink Link

linkURIs ::
  ( All Unconstrained (EndpointsT api)
  , All IsLink (ReturnTypes (MkLink api))
  , HasLink api
  , ApplyInputs api (MkLink api)
  ) => Proxy api -> Inputs (MkLink api) -> [URI]
linkURIs api = applyInputs api (Proxy @IsLink) (Proxy @Unconstrained) f (allLinks api)
  where
    f _endpoint link = [linkURI link]

-- | Run benchmarks for server handlers (one benchmark per API endpoint).
--
-- This relies on 'HasLink' to generate benchmark names.
benchHandlers ::
  ( HasLink api
  , All Unconstrained (EndpointsT api)
  , All NFDataHandler (ReturnTypes (Server api))
  , All IsLink (ReturnTypes (MkLink api))
  , ApplyInputs api (Server api)
  , ApplyInputs api (MkLink api)
  , Inputs (Server api) ~ Inputs (MkLink api)
  ) => Proxy api -> Server api -> Inputs (Server api) -> [Benchmark]
benchHandlers api server inputs = zipWith bench names benchmarkables
  where
    names = map show (linkURIs api inputs)
    benchmarkables = benchmarkableHandlers api server inputs

-- these should be in servant-server
deriving instance Generic ServantErr
instance NFData ServantErr
instance NFData NoContent

-- this is present in later servant
allLinks
    :: forall api. HasLink api
    => Proxy api
    -> MkLink api
allLinks api = toLink api (safeLink (Proxy @(Get '[JSON] NoContent))(Proxy @(Get '[JSON] NoContent)))

-- this is present in later servant
instance (HasLink a, HasLink b) => HasLink (a :<|> b) where
  type MkLink (a :<|> b) = MkLink a :<|> MkLink b
  toLink _ l = toLink (Proxy :: Proxy a) l :<|> toLink (Proxy :: Proxy b) l
