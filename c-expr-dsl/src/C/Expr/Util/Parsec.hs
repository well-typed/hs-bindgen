module C.Expr.Util.Parsec (
    -- * Character streams
    caseInsensitive'
  , foldCharTokens
  , satisfyWith
    -- * General purpose
  , Consumer(..)
  , foldTokens
  ) where

import Control.Monad (guard)
import Data.Char (toLower)
import Text.Parsec (Consumed (..), ParseError, ParsecT, Reply (..), SourcePos,
                    State (..), Stream (..), mkPT, tokenPrim, unknownError,
                    (<?>))
import Text.Parsec.Error (Message (..), newErrorMessage)
import Text.Parsec.Pos (updatePosChar, updatePosString)

{-------------------------------------------------------------------------------
  Character streams
-------------------------------------------------------------------------------}

-- | Case-insensitive version of 'string''
--
-- Returns the parsed string (which may be different from the argument string).
caseInsensitive' :: Stream s m Char => String -> ParsecT s u m String
caseInsensitive' = \expected ->
    foldCharTokens (go [] expected) <?> expected
  where
    go :: String -> String -> Consumer Char String
    go acc []     = Done (reverse acc)
    go acc (x:xs) = Look { onEof   = Nothing
                         , onToken = \y -> do
                             guard (toLower x == toLower y)
                             return $ go (y:acc) xs
                         }

-- | Specialization of 'foldTokens' to streams of characters
foldCharTokens :: Stream s m Char => Consumer Char a -> ParsecT s u m a
foldCharTokens = foldTokens show updatePosString

-- | Generalization of 'satisfy' that returns evidence
satisfyWith :: Stream s m Char => (Char -> Maybe a) -> ParsecT s u m a
satisfyWith =
    tokenPrim show updatePos
  where
    updatePos :: SourcePos -> Char -> s -> SourcePos
    updatePos pos c _ = updatePosChar pos c

{-------------------------------------------------------------------------------
  General purpose
-------------------------------------------------------------------------------}

data Consumer t a =
    Done a
  | Look {
        onEof   :: Maybe a
      , onToken :: t -> Maybe (Consumer t a)
      }

-- | Fold a sequence of tokens; no input is consumed on failure.
--
-- This is a generalization of 'tokens'', which can be defined in terms of
-- 'foldTokens' as follows:
--
-- > tokens' showTokens updatePos expected =
-- >     foldTokens showTokens updatePos (go expected) <?> showTokens expected
-- >   where
-- >     go :: [t] -> Consumer t [t]
-- >     go []     = Done expected
-- >     go (t:ts) = Look { onEof   = Nothing
-- >                      , onToken = \t' -> guard (t == t') >> return (go ts)
-- >                      }
foldTokens :: forall s u m t a.
     Stream s m t
  => ([t] -> String)
  -> (SourcePos -> [t] -> SourcePos)
  -> Consumer t a
  -> ParsecT s u m a
foldTokens showTokens updatePos = \f ->
    mkPT $ \st -> aux st f
  where
    aux :: State s u -> Consumer t a -> m (Consumed (m (Reply s u a)))
    aux initState =
        walk [] (stateInput initState)
      where
        walk :: [t] -> s -> Consumer t a -> m (Consumed (m (Reply s u a)))
        walk acc rs (Done a)             = ok acc rs a
        walk acc rs Look{onEof, onToken} = do
            mNextToken <- uncons rs
            case mNextToken of
              Nothing       -> case onEof of
                                 Nothing -> err errEof
                                 Just a  -> ok acc rs a
              Just (t, rs') -> case onToken t of
                                 Nothing -> err (errUnexpected t)
                                 Just k  -> walk (t:acc) rs' k

        ok :: [t] -> s -> a -> m (Consumed (m (Reply s u a)))
        ok acc rs a = return $
            (if null acc then Empty else Consumed) $
              return $ Ok a finalState (unknownError finalState)
          where
           finalState :: State s u
           finalState = State{
                 statePos   = updatePos (statePos initState) (reverse acc)
               , stateUser  = stateUser initState
               , stateInput = rs
               }

        err :: ParseError -> m (Consumed (m (Reply s u a)))
        err e = return $ Empty $ return $ Error $ e

        errEof :: ParseError
        errEof =
            newErrorMessage (SysUnExpect "") (statePos initState)

        errUnexpected :: t -> ParseError
        errUnexpected t =
           newErrorMessage (SysUnExpect $ showTokens [t]) (statePos initState)

_tokens' :: forall s u m t.
     (Stream s m t, Eq t)
  => ([t] -> String)
  -> (SourcePos -> [t] -> SourcePos)
  -> [t]
  -> ParsecT s u m [t]
_tokens' showTokens updatePos expected =
    foldTokens showTokens updatePos (go expected) <?> showTokens expected
  where
    go :: [t] -> Consumer t [t]
    go []     = Done expected
    go (t:ts) = Look { onEof   = Nothing
                     , onToken = \t' -> guard (t == t') >> return (go ts)
                     }
