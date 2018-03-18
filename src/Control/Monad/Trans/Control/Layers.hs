{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

#include "overlap.h"

{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-missing-methods #-}

module Control.Monad.Trans.Control.Layers
    ()
where

-- base ----------------------------------------------------------------------
import           Control.Monad ((>=>))
import           Unsafe.Coerce (unsafeCoerce)


-- layers --------------------------------------------------------------------
import           Control.Monad.Lift hiding (OuterEffects)


-- monad-control -------------------------------------------------------------
import qualified Control.Monad.Trans.Control as T


-- transformers-base ---------------------------------------------------------
import qualified Control.Monad.Base as T


------------------------------------------------------------------------------
instance __OVERLAPPABLE__
    ( T.MonadBase b m, MonadTrans t
    , Applicative (t m), Monad (t m)
    )
  =>
    T.MonadBase b (t m)
  where
    liftBase = lift . T.liftBase


------------------------------------------------------------------------------
instance __OVERLAPPABLE__
    ( T.MonadBaseControl b m, MonadTransControl t, Monad (t m)
    )
  =>
    T.MonadBaseControl b (t m)
  where
    liftBaseWith f = liftControl $ \peel -> T.liftBaseWith $ \peelM ->
        f $ \t -> unsafeCoerce $ peelM $ peel t

    restoreM = lift . T.restoreM . unsafeCoerce >=> resume
