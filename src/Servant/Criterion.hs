{-# LANGUAGE ConstraintKinds #-}
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

data a :-> b = a :-> b

-- | Figure out all inputs to all endpoint handlers of some API.
--
-- >>> type SampleAPI = QueryParam "session_id" Int :> Get '[JSON] String
-- >>> :kind! Server
-- Server        ServerInputs  ServerT
-- >>> :kind! ServerInputs SampleAPI
-- ServerInputs SampleAPI :: *
-- = (Maybe Int, ())
type ServerInputs api = HandlerInputs (Server api)

type family HandlerInputs server where
  HandlerInputs (a :<|> b)  = HandlerInputs a :<|> HandlerInputs b
  HandlerInputs (a -> b)    = a :-> HandlerInputs b
  HandlerInputs a           = ()

type family ReturnTypes server where
  ReturnTypes (a :<|> b)  = B (ReturnTypes a) (ReturnTypes b)
  ReturnTypes (a -> b)    = ReturnTypes b
  ReturnTypes (Handler a) = L a

data T a = L a | B (T a) (T a)

type family Endpoints' api where
  Endpoints' (a :<|> b) = B (Endpoints' a) (Endpoints' b)
  Endpoints' (a :> b) = PrependParam a (Endpoints' b)
  Endpoints' a = L a

type family PrependParam param api where
  PrependParam param (B a b) = B (PrependParam param a) (PrependParam param b)
  PrependParam param (L a) = L (param :> a)

type family All c xs :: Constraint where
  All c (B a b) = (All c a, All c b)
  All c (L a) = c a

class ServerXXX api server where
  serverXXX
    :: (Monoid m, All c (ReturnTypes server), All ce (Endpoints' api))
    => Proxy (api :: *)
    -> Proxy c
    -> Proxy ce
    -> (forall (endpoint :: *) x. (c x, ce endpoint) => Proxy endpoint -> Handler x -> m)
    -> server
    -> HandlerInputs server
    -> m

instance Endpoints' endpoint ~ L endpoint => ServerXXX endpoint (Handler a) where
  serverXXX api _ _ f server () = f api server

instance ServerXXX endpoint api => ServerXXX endpoint (param -> api) where
  serverXXX api pc pce f server (a :-> inputs) = serverXXX api pc pce f (server a) inputs

instance (ServerXXX ea a, ServerXXX eb b) => ServerXXX (ea :<|> eb) (a :<|> b) where
  serverXXX _ pc pce f (sa :<|> sb) (a :<|> b)
    = serverXXX (Proxy @ea) pc pce f sa a <> serverXXX (Proxy @eb) pc pce f sb b

class Unconstrained endpoint
instance Unconstrained endpoint

benchHandlers ::
  ( All Unconstrained (Endpoints' api)
  , All NFData (ReturnTypes server)
  , ServerXXX api server
  ) => Proxy api -> server -> HandlerInputs server -> [Benchmark]
benchHandlers api = serverXXX api (Proxy @NFData) (Proxy @Unconstrained) f
  where
    f _endpoint handler = [bench "endpoint" (nfIO (runHandler handler))]

-- these should be in servant-server
deriving instance Generic ServantErr
instance NFData ServantErr
