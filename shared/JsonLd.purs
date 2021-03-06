module Shared.JsonLd
       (
         Context
       , ContextDefinition(..)
       , ContextValue(..)
       , ExpandedTermDefinition
       , Node(..)
       , NodeMetadata
       , NodeMetadata'
       , NodeMetadata''
       , unwrapNode

       , _unwrappedNode
       , _id
       , _resource

       ) where

import Prelude

import Control.Alt ((<|>))
import Control.Apply (lift2)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (SProxy(..))
import Foreign (F, Foreign)
import Prim.Row as Row
import Record as Record
import Shared.Common (Url)
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)

-- TODO - should be able to do magic rename stuff on ContextFields - will need typeclass similar to record-diff
-- TODO - should be able to assert via type class that fields in ContextFields exist in outer resource

data ContextValue = ExpandedTermDefinition ExpandedTermDefinition
                  | Other String
derive instance eqContextValue :: Eq ContextValue
derive instance genericContextValue :: Generic ContextValue _
instance showContextValue :: Show ContextValue where show = genericShow

instance writeForeignContextValue :: WriteForeign ContextValue where
  writeImpl (ExpandedTermDefinition a) = writeImpl a
  writeImpl (Other a) = writeImpl a

instance readForeignContextValue :: ReadForeign ContextValue where
  readImpl o =
    let
      readExpandedTerm = readImpl :: Foreign -> F ExpandedTermDefinition
      readOther = readImpl :: Foreign -> F String
    in
     (ExpandedTermDefinition <$> readExpandedTerm o)
     <|> (Other <$> readOther o)

type ContextFields extraFields = { "@language" :: Maybe String
                                 , "@base" :: Maybe String
                                 , "@vocab" :: Maybe String
                                 | extraFields
                                 }

newtype Context extraFields = Context (ContextFields extraFields)

derive instance newtypeContext :: Newtype (Context extraFields) _
derive instance eqContext :: (Eq (ContextFields extraFields)) => Eq (Context extraFields)
derive instance genericContext :: Generic (Context extraFields) _
instance showContext :: (Show (ContextFields a)) => Show (Context a) where
  show (Context a) = "Context:" <> show a

instance writeForeignContext :: (WriteForeign (ContextFields extraFields)) => WriteForeign (Context extraFields) where
   writeImpl (Context fields) = writeImpl fields

instance readForeignContext :: (ReadForeign (ContextFields extraFields)) => ReadForeign (Context extraFields) where
   readImpl o = Context <$> readImpl o

type ExpandedTermDefinition = { "@id" :: Maybe String
                              , "@reverse" :: Maybe String
                              , "@valueType" :: Maybe String
                              , "@language" :: Maybe String
                              , "@container" :: Maybe String
                              }

data ContextDefinition contextFields = ContextRecord (Context contextFields)
                                     | ContextUrl String
derive instance eqContextDefinition :: (Eq (Context contextFields)) => Eq (ContextDefinition contextFields)
derive instance genericContextDefinition :: Generic (ContextDefinition contextFields) _
instance showContextDefinition :: (Show (Context contextFields)) => Show (ContextDefinition contextFields) where show = genericShow

instance writeForeignContextDefinition :: (WriteForeign (ContextFields contextFields)) => WriteForeign (ContextDefinition contextFields) where
  writeImpl (ContextRecord r) = writeImpl r
  writeImpl (ContextUrl url) = writeImpl url

instance readForeignContextDefinition :: (ReadForeign (ContextFields contextFields)) => ReadForeign (ContextDefinition contextFields) where
  readImpl o =
    let
      readRecord = readImpl :: Foreign -> F (ContextFields contextFields)
      readUrl = readImpl :: Foreign -> F String
    in
     (ContextRecord <<< Context <$> readRecord o)
     <|> (ContextUrl <$> readUrl o)

type NodeMetadata'' resource contextFields = ( "@id" :: Maybe Url
                                             , "@nodeType":: Maybe String
                                             , "@context" :: Maybe (ContextDefinition contextFields)
                                               | resource )

type NodeMetadata' resource contextFields = Record (NodeMetadata'' resource contextFields)

type NodeMetadata resource contextFields = NodeMetadata' (resource :: resource) contextFields

newtype Node resource contextFields = Node (NodeMetadata resource contextFields)

derive instance newtypeNode :: Newtype (Node a b) _
derive newtype instance eqNode :: (Eq a, Eq (ContextFields contextFields)) => Eq (Node a contextFields)
derive newtype instance showNode :: (Show a, Show (ContextFields contextFields)) => Show (Node a contextFields)

instance writeForeignNode :: ( Row.Union r1 (NodeMetadata''() contextFields) r2
                             , Row.Nub r2 r3
                             , WriteForeign (Record r3)
                             )
                             => WriteForeign (Node (Record r1) contextFields) where
  writeImpl (Node metadata@{resource}) = writeImpl $ ((mergeResourceAndMetadata metadata resource) :: Record r3)
else instance writeForeignNode1 :: ( Newtype newtypeType (Record r1)
                                   , Row.Union r1 (NodeMetadata''() contextFields) r2
                                   , Row.Nub r2 r3
                                   , WriteForeign (Record r3)
                                   )
                                   => WriteForeign (Node newtypeType contextFields) where
  writeImpl (Node metadata@{resource: newTypeResource}) = writeImpl $ ((mergeResourceAndMetadata metadata (unwrap newTypeResource)) :: Record r3)

instance readForeignNode :: (ReadForeign a
                            , ReadForeign (NodeMetadata'() contextFields)
                            )
                            => ReadForeign (Node a contextFields) where
  readImpl o =
    let
      resource :: F a
      resource = readImpl o

      metadataWithPrefix :: F (NodeMetadata'() contextFields)
      metadataWithPrefix = readImpl o

      metadataWithResource :: F (NodeMetadata a contextFields)
      metadataWithResource = lift2 (Record.insert (SProxy :: SProxy "resource")) resource metadataWithPrefix

    in
     Node <$> metadataWithResource

mergeResourceAndMetadata :: forall a b r r2 r3. Row.Lacks "resource" r => Row.Union a r r2 => Row.Nub r2 r3 => {resource :: b | r} -> Record a -> Record r3
mergeResourceAndMetadata metadata resource =
    let
      metadataWithoutResource = Record.delete (SProxy :: SProxy "resource") metadata
      merged = Record.merge resource metadataWithoutResource
    in
     merged

unwrapNode :: forall a b. Node a b -> a
unwrapNode (Node {resource}) = resource

--------------------------------------------------------------------------------
-- Lenses
--------------------------------------------------------------------------------
_unwrappedNode :: forall resource contextFields. Lens' (Node resource contextFields) (NodeMetadata resource contextFields)
_unwrappedNode = _Newtype

_id :: forall resource contextFields. Lens' (NodeMetadata resource contextFields) (Maybe Url)
_id = prop (SProxy :: SProxy "@id")

_resource :: forall resource contextFields. Lens' (NodeMetadata resource contextFields) resource
_resource = prop (SProxy :: SProxy "resource")
