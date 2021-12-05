{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Control.Exception.Checked (Throw(..), Throws, Catch(..), boom, Boom, Error) where

import qualified Control.Exception as Exc
import Unsafe.Coerce ( unsafeCoerce )

class Throws e where

class Throw m where
  throwChecked :: (Exc.Exception e, Throws e) => e -> m a
  throwChecked = throwUnchecked
  throwUnchecked :: (Exc.Exception e) => e -> m a

class Catch m where
  catch :: forall e a. Exc.Exception e => (Throws e => m a) -> (e -> m a) -> m a
  tryCatch :: (Exc.Exception e, Throws e) => m a -> (e -> m a) -> m a

instance Throw IO where
  throwChecked = Exc.throwIO
  throwUnchecked = Exc.throwIO

instance Catch IO where
  catch :: forall e a. Exc.Exception e => (Throws e => IO a) -> (e -> IO a) -> IO a
  catch a c = Exc.catch (hide @e a) c
  tryCatch a c = Exc.catch a c

type Error m = (Throw m, Catch m)
    
newtype a ==> b = Hide (a => b)

type family Boom es where
  Boom (e ': es) = (Throws e, Boom es)
  Boom '[] = ()

hide :: forall e a. (Throws e => a) -> a
hide = boom @'[e]

boom :: forall es a. (Boom es => a) -> a
boom x = unsafeCoerce (Hide x :: Boom es ==> a) ()
