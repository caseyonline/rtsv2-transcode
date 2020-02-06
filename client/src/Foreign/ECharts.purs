module Foreign.ECharts where

import Prelude

import Control.Monad.Except (throwError)
import Data.Either (Either)
import Data.Generic.Rep (class Generic, Constructor(..), NoArguments(..), Sum(..), to)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign as Foreign
import Foreign.Object (Object)
import Halogen as H
import Prim.Row as Row
import Record as Record
import Shared.Types (PoPName)
import Simple.JSON as JSON
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.HTML (HTMLElement, window)
import Web.HTML.HTMLDocument (toParentNode)
import Web.HTML.HTMLElement (fromElement)
import Web.HTML.Window (document)


foreign import data Instance :: Type

foreign import makeChart_ :: HTMLElement -> Effect Instance

foreign import setOption_ :: forall option. option -> Instance -> Effect Unit

-- main function
makeChart
  :: HTMLElement
  -> Effect Instance
makeChart = makeChart_

setOption
  :: forall option option'
   . Row.Union option option' Option
  => Record option
  -> Instance
  -> Effect Unit
setOption = setOption_

-- types
type Option =
  ( title :: TitleOption
  , tooltip :: TooltipOption
  , backgroundColor :: String
  , color :: Array String
  , xAxis :: XAxisOption
  , yAxis :: YAxisOption
  , visualMap :: VisualMapOption
  , calendar :: CalendarOption
  , series :: Array SeriesOption
  )

data TitleOption
data TooltipOption
data XAxisOption
data YAxisOption
data VisualMapOption
data CalendarOption
data SeriesOption

type Title =
  ( text :: String
  )

type Tooltip =
  ( position :: String
  , trigger :: String
  )

type Legend =
  ( orient :: String
  , x :: Either String Int
  , y :: Either String Int
  , data :: Array String
  , selectedMode :: String
  , selected :: Selected String
  )

type Selected a =
  { a :: String }

type XAxis =
  ( data :: Array String
  )

type YAxis =
  ( data :: Array String
  )

type VisualMap =
  ( min :: Number
  , max :: Number
  , calculable :: Boolean
  , orient :: String
  , left :: String
  , bottom :: String
  )

type Calendar =
  ( cellSize :: Array String
  , range :: Array String
  )

type MapSeries =
  ( name :: String
  , type :: String
  , mapType :: String
  , roam :: Boolean
  , hoverable :: Boolean
  , markLine :: MarkLine
  , data :: Maybe (Array (Array Int))
  )

type MarkLine =
  { smooth :: Boolean
  , effect :: MarkLineEffect
  , itemStyle :: Record ItemStyle
  , data :: Array (Array PoPName)
  }

type MarkLineEffect =
  { show :: Boolean
  , size :: Int
  , showdowColor :: String
  }

type ItemStyle =
  ( normal :: { borderWidth :: Int }
  )

type Journey =
  ( name :: String
  , value :: Int
  )

type BarSeries =
  ( name :: String
  , data :: Array Number
  )

type HeatMapSeries =
  ( name :: String
  , data :: Array (Array String)
  , coordinateSystem :: String
  , calendarIndex :: Int
  )

-- helpers
makeTitle
  :: forall fields fields'
   . Row.Union fields fields' Title
  => { | fields }
  -> TitleOption
makeTitle = unsafeCoerce

makeTooltip
  :: forall fields fields'
   . Row.Union fields fields' Tooltip
  => { | fields }
  -> TooltipOption
makeTooltip = unsafeCoerce

makeXAxis
  :: forall fields fields'
   . Row.Union fields fields' XAxis
  => { | fields }
  -> XAxisOption
makeXAxis = unsafeCoerce

makeYAxis
  :: forall fields fields'
   . Row.Union fields fields' YAxis
  => { | fields }
  -> YAxisOption
makeYAxis = unsafeCoerce

makeVisualMap
  :: forall fields fields'
   . Row.Union fields fields' VisualMap
  => { | fields }
  -> VisualMapOption
makeVisualMap = unsafeCoerce

makeCalendar
  :: forall fields fields'
   . Row.Union fields fields' Calendar
  => { | fields }
  -> CalendarOption
makeCalendar = unsafeCoerce

makeBarSeries
  :: forall fields fields' trash
   . Row.Union fields fields' BarSeries
  => Row.Lacks "type" fields
  => Row.Cons "type" String fields trash
  => { | fields }
  -> SeriesOption
makeBarSeries r = unsafeCoerce $ Record.insert (SProxy :: SProxy "type") "bar" r

makeMapSeries
  :: forall fields fields' trash
   . Row.Union fields fields' MapSeries
  => Row.Lacks "type" fields
  => Row.Cons "type" String fields trash
  => { | fields }
  -> SeriesOption
makeMapSeries r = unsafeCoerce $ Record.insert (SProxy :: SProxy "type") "map" r

makeHeatMapSeries
  :: forall fields fields' trash
   . Row.Union fields fields' HeatMapSeries
  => Row.Lacks "type" fields
  => Row.Cons "type" String fields trash
  => { | fields }
  -> SeriesOption
makeHeatMapSeries r = unsafeCoerce $ Record.insert (SProxy :: SProxy "type") "heatmap" r

getElementById :: String -> Aff (Maybe HTMLElement)
getElementById s =
    H.liftEffect
        $ window
            >>= document
            >>= map (_ >>= fromElement)
                <<< querySelector (QuerySelector s)
                <<< toParentNode
