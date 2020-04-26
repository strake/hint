{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}

module Control.Monad.Ghc (
    GhcT, runGhcT
) where

import Prelude
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.IORef

import qualified GHC
import qualified MonadUtils as GHC
import qualified Exception as GHC
import qualified GhcMonad as GHC

import qualified DynFlags as GHC

newtype GhcT m a = GhcT (GHC.GhcT (MTLAdapter m) a)
  deriving (Functor, Applicative, Monad, GHC.HasDynFlags)
  deriving (MonadThrow, MonadIO, MonadCatch, MonadMask) via ReaderT GHC.Session m
  deriving (MonadTrans) via ReaderT GHC.Session

-- adapted from https://github.com/ghc/ghc/blob/ghc-8.2/compiler/main/GHC.hs#L450-L459
-- modified to _not_ catch ^C
runGhcT :: (MonadIO m, MonadMask m) => Maybe FilePath -> GhcT m a -> m a
runGhcT mb_top_dir (GhcT ghct) = unMTLA $ do
  ref <- liftIO $ newIORef (error "empty session")
  let session = GHC.Session ref
  flip GHC.unGhcT session $ {-GHC.withSignalHandlers $-} do -- do _not_ catch ^C
    GHC.initGhcMonad mb_top_dir
    GHC.withCleanupSession ghct

instance (MonadIO m, MonadCatch m, MonadMask m) => GHC.ExceptionMonad (GhcT m) where
    gcatch = catch
    gmask  = mask

deriving newtype instance (MonadIO m, MonadCatch m, MonadMask m) => GHC.GhcMonad (GhcT m)

-- | We use the 'MTLAdapter' to convert between similar classes
--   like 'MTL'''s 'MonadIO' and 'GHC'''s 'MonadIO'.
newtype MTLAdapter m a = MTLAdapter {unMTLA :: m a} deriving (Functor, Applicative, Monad)

instance MonadIO m => GHC.MonadIO (MTLAdapter m) where
    liftIO = MTLAdapter . liftIO

instance (MonadIO m, MonadCatch m, MonadMask m) => GHC.ExceptionMonad (MTLAdapter m) where
  m `gcatch` f = MTLAdapter $ unMTLA m `catch` (unMTLA . f)
  gmask io = MTLAdapter $ mask (\f -> unMTLA $ io (MTLAdapter . f . unMTLA))
