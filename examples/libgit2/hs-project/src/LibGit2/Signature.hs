-- | Converting between the high-level 'Signature' and the generated
-- @git_signature@ struct, in both directions.
--
-- The read side ('sigUnmarshal') uses 'UnmarshalStruct' to copy a borrowed
-- @const git_signature *@ out into 'Text' fields. The write side ('sigMarshal')
-- uses 'MarshalStruct' to write a 'Signature' into a @git_signature@ for
-- @git_commit_create@. Together they exercise both halves of the struct
-- vocabulary, including the nested @git_time@.
--
module LibGit2.Signature
  ( sigUnmarshal
  , peekSignatureConst
  , sigMarshal
  ) where

import Foreign.C.Types (CChar)
import Foreign.Storable (peek)

import HsBindgen.Runtime.HighLevel.Marshaller (MarshalStruct, UnmarshalStruct,
                                               at, marshalNested,
                                               runUnmarshalStruct, scalar,
                                               struct, unmarshalField,
                                               unmarshalFieldPure,
                                               unmarshalNested, (>>>))
import HsBindgen.Runtime.PtrConst (PtrConst)
import HsBindgen.Runtime.PtrConst qualified as PtrConst

import Generated.Types (Git_signature (..), Git_time (..))
import LibGit2.Marshal (peekText, textInPtr)
import LibGit2.Types (GitTime (..), Signature (..))

{-------------------------------------------------------------------------------
  Read: git_signature -> Signature
-------------------------------------------------------------------------------}

timeUnmarshal :: UnmarshalStruct Git_time GitTime
timeUnmarshal =
      GitTime
  <$> unmarshalFieldPure git_time_time   fromIntegral
  <*> unmarshalFieldPure git_time_offset fromIntegral

sigUnmarshal :: UnmarshalStruct Git_signature Signature
sigUnmarshal =
      Signature
  <$> unmarshalField  git_signature_name  peekText
  <*> unmarshalField  git_signature_email peekText
  <*> unmarshalNested git_signature_when  timeUnmarshal

-- | Peek and decode a borrowed @const git_signature *@.
peekSignatureConst :: PtrConst Git_signature -> IO Signature
peekSignatureConst p = peek (PtrConst.unsafeToPtr p) >>= runUnmarshalStruct sigUnmarshal

{-------------------------------------------------------------------------------
  Write: Signature -> git_signature
-------------------------------------------------------------------------------}

timeMarshal :: MarshalStruct GitTime Git_time
timeMarshal =
    struct Git_time
      (     at gitTimeEpoch     (scalar fromIntegral)  -- time
        >>> at gitTimeOffsetMin (scalar fromIntegral)  -- offset
        >>> scalar signChar                            -- sign
      )
  where
    signChar gt = if gitTimeOffsetMin gt < 0 then minus else plus
    plus, minus :: CChar
    plus  = 43  -- '+'
    minus = 45  -- '-'

sigMarshal :: MarshalStruct Signature Git_signature
sigMarshal =
    struct Git_signature
      (     at sigName  textInPtr
        >>> at sigEmail textInPtr
        >>> at sigWhen  (marshalNested timeMarshal)
      )
