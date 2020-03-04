module Shared.JsonLd
       (
         Context
       , ContextValue(..)
       , ExpandedTermDefinition
       , Node
       ) where

import Prelude

import Control.Alt ((<|>))
import Control.Apply (lift2)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Foreign (F, Foreign)
import Prim.Row as Row
import Record as Record
import Shared.Common (Url)
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)

-- TODO - filter 'undefined' out of json in Json.erl - matches javascript implementation
-- TODO - should be able to do magic rename stuff on ContextFields - will need typeclass similar to record-diff
-- TODO - should be able to assert via type class that fields in ContextFields exist in outer resource

data ContextValue = ExpandedTermDefinition ExpandedTermDefinition
                  | Other String

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

type NodeMetadata'' resource contextFields = ( "@id" :: Maybe Url
                                             , "@nodeType":: Maybe String
                                             , "@context" :: Maybe (Context contextFields)
                                               | resource )

type NodeMetadata' resource contextFields = Record (NodeMetadata'' resource contextFields)

type NodeMetadata resource contextFields = NodeMetadata' (resource :: resource) contextFields

newtype Node resource contextFields = Node (NodeMetadata resource contextFields)

derive instance newtypeNode :: Newtype (Node a b) _


instance writeForeignNode :: ( Row.Union r1 (NodeMetadata''() contextFields) r2
                             , Row.Nub r2 r3
                             , WriteForeign (Record r3)
                             )
                             => WriteForeign (Node (Record r1) contextFields) where
  writeImpl (Node metadata@{resource}) =
    let
      metadataWithoutResource :: NodeMetadata' () contextFields
      metadataWithoutResource = Record.delete (SProxy :: SProxy "resource") metadata

      merged :: Record r3
      merged = Record.merge resource metadataWithoutResource
    in
      writeImpl merged

instance readForeignNode :: (ReadForeign a
                            , ReadForeign (NodeMetadata'() contextFields)
                            )
                            => ReadForeign (Node a contextFields) where
  readImpl o =
    let
      resource :: F a
      resource = readImpl o

      metadataWithPrefix :: F (NodeMetadata'() contextFields) -- todo - should have contextfields or it won't recurse deep enough
      metadataWithPrefix = readImpl o

      metadataWithResource :: F (NodeMetadata a contextFields)
      metadataWithResource = lift2 (Record.insert (SProxy :: SProxy "resource")) resource metadataWithPrefix

    in
     Node <$> metadataWithResource
