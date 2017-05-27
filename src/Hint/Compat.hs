module Hint.Compat where

import qualified Hint.GHC as GHC

-- Kinds became a synonym for Type in GHC 6.8. We define this wrapper
-- to be able to define a FromGhcRep instance for both versions
newtype Kind = Kind GHC.Kind

supportedExtensions :: [String]
supportedExtensions = map f GHC.xFlags
    where
#if (__GLASGOW_HASKELL__ >= 710)
      f = GHC.flagSpecName
#else
      f (e,_,_) = e
#endif

configureDynFlags :: GHC.DynFlags -> GHC.DynFlags
configureDynFlags dflags =
    (if GHC.dynamicGhc then GHC.addWay' GHC.WayDyn else id)
                           dflags{GHC.ghcMode    = GHC.CompManager,
                                  GHC.hscTarget  = GHC.HscInterpreted,
                                  GHC.ghcLink    = GHC.LinkInMemory,
                                  GHC.verbosity  = 0}

parseDynamicFlags :: GHC.GhcMonad m
                  => GHC.DynFlags -> [String] -> m (GHC.DynFlags, [String])
parseDynamicFlags d = fmap firstTwo . GHC.parseDynamicFlags d . map GHC.noLoc
    where firstTwo (a,b,_) = (a, map GHC.unLoc b)

pprType :: GHC.Type -> GHC.SDoc
pprType = GHC.pprTypeForUser
