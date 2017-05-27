module Hint.Compat where

import qualified Hint.GHC as GHC

supportedExtensions :: [String]
supportedExtensions = map f GHC.xFlags
    where
#if (__GLASGOW_HASKELL__ >= 710)
      f = GHC.flagSpecName
#else
      f (e,_,_) = e
#endif
