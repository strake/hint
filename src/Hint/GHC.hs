module Hint.GHC (
    Message, module X,
#if __GLASGOW_HASKELL__ < 804
    GhcPs, mgModSummaries
#endif
) where

import GHC as X hiding (Phase, GhcT, runGhcT)
import Control.Monad.Ghc as X (GhcT, runGhcT)

import HscTypes as X (SourceError, srcErrorMessages, GhcApiError)
#if __GLASGOW_HASKELL__ >= 804
import HscTypes as X (mgModSummaries)
#endif

import Outputable as X (PprStyle, SDoc, Outputable(ppr),
                        showSDoc, showSDocForUser, showSDocUnqual,
                        withPprStyle, defaultErrStyle)

import ErrUtils as X (mkLocMessage, pprErrMsgBagWithLoc, MsgDoc) -- we alias MsgDoc as Message below

import DriverPhases as X (Phase(Cpp), HscSource(HsSrcFile))
import StringBuffer as X (stringToStringBuffer)
import Lexer as X (P(..), ParseResult(..), mkPState)
import Parser as X (parseStmt, parseType)
import FastString as X (fsLit)

import DynFlags as X (xFlags, xopt, LogAction, FlagSpec(..),
                      WarnReason(NoReason), addWay', Way(..), dynamicGhc)

import PprTyThing as X (pprTypeForUser)
import SrcLoc as X (mkRealSrcLoc)

import ConLike as X (ConLike(RealDataCon))

type Message = MsgDoc

#if __GLASGOW_HASKELL__ < 804
type GhcPs = RdrName

mgModSummaries :: ModuleGraph -> [ModSummary]
mgModSummaries = id
#endif
