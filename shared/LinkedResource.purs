module Shared.LinkedResource
       (
         LinkedResource
       , PrefixProps(..)
       ) where

import Data.Newtype (class Newtype, wrap)
import Data.Symbol (class IsSymbol, SProxy(..))
import Foreign (F)
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Prelude (flip, identity, ($), (<$>), (<<<), (>>>))
import Prim.Row as Row
import Prim.Symbol (class Append)
import Record (delete, merge)
import Record.Builder (Builder)
import Record.Builder as Builder
import Shared.Types (Url)
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)

type LinkMetadata'' a = ( id :: Url | a )

type LinkMetadata' a = Record (LinkMetadata'' a)

type LinkMetadata a = LinkMetadata' (resource :: a)

newtype LinkedResource a = LinkedResource (LinkMetadata a)
derive instance newtypeLinkedResource :: Newtype (LinkedResource a) _

data PrefixProps sym = PrefixProps (SProxy sym)

instance prefixProps :: ( IsSymbol newsym
                        , Append presym sym newsym
                        , Row.Lacks newsym rb
                        , Row.Cons newsym a rb rc
                        ) => FoldingWithIndex (PrefixProps presym) (SProxy sym) (Builder { | ra } { | rb }) a (Builder { | ra } { | rc })
  where
  foldingWithIndex (PrefixProps _) _ rin a = (_ >>> Builder.insert (SProxy :: SProxy newsym) a) rin

-- | Adds a common prefix to a Record's labels.
add :: forall rin rout pre. HFoldlWithIndex (PrefixProps pre) (Builder {} {}) { | rin } (Builder {} { | rout }) => SProxy pre -> { | rin } -> { | rout }
add pre = flip Builder.build {} <<< hfoldlWithIndex (PrefixProps pre) (identity :: Builder {} {})

instance writeForeignLinkedResource :: ( HFoldlWithIndex (PrefixProps "@") (Builder {} {}) (LinkMetadata'()) (Builder {} (Record lm'))
                                       , Row.Union r1 lm' r2
                                       , Row.Nub r2 r3
                                       , WriteForeign (Record r3)
                                       )
                                       => WriteForeign (LinkedResource (Record r1)) where
  writeImpl (LinkedResource metadata@{resource}) =
    let
      deleted :: LinkMetadata'()
      deleted = delete (SProxy :: SProxy "resource") metadata

      added :: Record lm'
      added = add (SProxy :: SProxy "@") $ deleted

      merged :: Record r3
      merged = merge resource added
    in
      writeImpl merged

instance readForeignLinkedResource :: ReadForeign a => ReadForeign (LinkedResource a) where
  readImpl o =
    let
      resource :: F a
      resource = readImpl o
    in
     (\r -> LinkedResource { id : wrap "url here"
                           , resource: r
                           }) <$> resource
