module Main where

import Prelude
import qualified Control.Monad.Eff.Console as C
import Control.Monad.Eff (Eff ())
import Control.Monad.Eff.Exception
import Control.Monad.Trans (lift)
import Control.Monad.Maybe.Trans
import qualified Control.Monad.Eff.JQuery as J
import Data.Either (Either (Left, Right))
import Data.Lens
import Data.Lens.Setter
import Data.JSON
import Data.Maybe
import Data.Traversable
import DOM
import Graphics.Canvas

import ChartJs
import WebSocket

main :: forall eff. Eff (ws :: WS, console :: C.CONSOLE, dom :: DOM, canvas::Canvas, err :: EXCEPTION | eff) Unit
main = do
  C.log "START"
  runWebSocket $ do
    result <- withWebSocket config handlers
    case result of
         Right _ -> output "DONE"
         Left err -> output err

config :: WebSocketConfig
config =
  { uri: "ws://127.0.0.1:9001"
  , protocols: []
  }

handlers :: forall eff. WebSocketHandler (console :: C.CONSOLE, dom :: DOM , err :: EXCEPTION, canvas :: Canvas | eff)
handlers s = (defaultHandlers s)
  { onOpen = onOpenHandler
  , onMessage = onMessageHandler
  }

onOpenHandler :: forall eff. WithWebSocket (console :: C.CONSOLE, dom :: DOM , err :: EXCEPTION, canvas :: Canvas | eff) Unit
onOpenHandler = C.log "OPEN"

onMessageHandler :: forall eff. String -> WithWebSocket (console :: C.CONSOLE, dom :: DOM , err :: EXCEPTION, canvas :: Canvas | eff) Unit
onMessageHandler msg = do
  case (decode msg :: Maybe Cpu) of
    Just c -> do
      initGraph "bar" $ initBar c
      C.log $ "Successfully parsed" ++ show c
    Nothing -> C.log "Failed to parse message"
  return unit

output :: forall eff. String -> WebSocket (console :: C.CONSOLE | eff) Unit
output = lift <<< C.log

initGraph
  :: forall eff
   . String
  -> (Chart -> Eff( dom :: DOM , err :: EXCEPTION, canvas :: Canvas | eff ) ChartType)
  -> Eff ( dom :: DOM , err :: EXCEPTION, canvas :: Canvas | eff ) Unit
initGraph name f = void $ do
  cMay  <- getCanvasElementById cId
  c     <- maybe (die $ "Could not find canvas: " <> cId) pure cMay
  ctx   <- getContext2D c
  chart <- newChart ctx
  ct    <- f chart
  lt    <- generateLegend ct
  leg   <- J.select $ "#" <> lId
  J.appendText lt leg
  where
    cId = name <> "-canvas"
    lId = name <> "-legend"

initBar
  :: forall eff
   . Cpu
  -> Chart
  -> Eff ( dom :: DOM , err :: EXCEPTION, canvas:: Canvas | eff ) ChartType
initBar (Cpu {cpuData = cpuData}) c =
  barChart c barData (responsiveChartConfig cpuBarChartCfg)
  where
    barData = {
      labels : ["user","nice","system","idle","iowait","irq","softirq", "steal"],
      datasets : [
        { fillColor : "rgba(2,136,209,0.75)"
        , strokeColor : "rgba(220,220,220,0.8)"
        , highlightFill: "rgba(255,82,82,0.75)"
        , highlightStroke: "rgba(220,220,220,1)"
        , data : cpuData
        }
        ]}

hmonChartCfg :: ChartConfig
hmonChartCfg = maintainAspectRatio .~ true $ animation .~ false $ defGlobalChartConfig

hmonBarChartCfg :: BarChartConfig
hmonBarChartCfg = global .~ hmonChartCfg $ legendTemplate .~ "" $ defBarChartConfig

cpuBarChartCfg :: BarChartConfig
cpuBarChartCfg = fixScale 10.0 hmonBarChartCfg

animation :: forall a b r. Lens { animation :: a | r } { animation :: b | r } a b
animation = lens _.animation (_ { animation = _ })

global :: forall a b r. Lens { global :: a | r } { global :: b | r } a b
global = lens _.global (_ { global = _ })

legendTemplate :: forall a b r. Lens { legendTemplate :: a | r } { legendTemplate :: b | r } a b
legendTemplate = lens _.legendTemplate (_ { legendTemplate = _ })

maintainAspectRatio :: forall a b r. Lens { maintainAspectRatio :: a | r } { maintainAspectRatio :: b | r } a b
maintainAspectRatio = lens _.maintainAspectRatio (_ { maintainAspectRatio = _ })

canvas :: forall a b r. Lens { canvas :: a | r } { canvas :: b | r } a b
canvas = lens _.canvas (_ { canvas = _ })

width :: forall a b r. Lens { width :: a | r } { width :: b | r } a b
width = lens _.width (_ { width = _ })

height :: forall a b r. Lens { height :: a | r } { height :: b | r } a b
height = lens _.height (_ { height = _ })

fixScale :: forall r
  . Number
  -> { global :: ChartConfig | r }
  -> { global :: ChartConfig | r }
fixScale steps a =  a { global = a.global {
  scaleOverride = true,
  scaleSteps= steps,
  scaleStepWidth= 10.0,
  scaleStartValue= 0.0
  } }

die :: forall eff a. String -> Eff( err:: EXCEPTION | eff ) a
die = error >>> throwException

data Cpu = Cpu {
              cpuData :: Array Number
           }

instance showCpu :: Show Cpu where
  show (Cpu cData) = "CPU: "

instance cpuFromJSON :: FromJSON Cpu where
  parseJSON (JObject o) = do
    d <- o .: "cpu"
    return $ Cpu {cpuData: d}
