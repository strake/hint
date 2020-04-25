module Hint.GHC (
    Message, module X
) where

import GHC as X hiding (Phase, GhcT, runGhcT)
import Control.Monad.Ghc as X (GhcT, runGhcT)

import HscTypes as X (SourceError, srcErrorMessages, GhcApiError)
import HscTypes as X (mgModSummaries)

import Outputable as X (PprStyle, SDoc, Outputable(ppr),
                        showSDoc, showSDocForUser, showSDocUnqual,
                        withPprStyle, defaultErrStyle, vcat)

import ErrUtils as X (mkLocMessage, pprErrMsgBagWithLoc, MsgDoc
#if __GLASGOW_HASKELL__ >= 810
  , errMsgSpan, pprErrMsgBagWithLoc
#endif
  ) -- we alias MsgDoc as Message below

import DriverPhases as X (Phase(Cpp), HscSource(HsSrcFile))
import StringBuffer as X (stringToStringBuffer)
import Lexer as X (P(..), ParseResult(..), mkPState
#if __GLASGOW_HASKELL__ >= 810
  , getErrorMessages
#endif
  )
import Parser as X (parseStmt, parseType)
import FastString as X (fsLit)

import DynFlags as X (xFlags, xopt, LogAction, FlagSpec(..),
                      WarnReason(NoReason), addWay', Way(..), dynamicGhc)

import PprTyThing as X (pprTypeForUser)
import SrcLoc as X (combineSrcSpans, mkRealSrcLoc)

import ConLike as X (ConLike(RealDataCon))

type Message = MsgDoc
