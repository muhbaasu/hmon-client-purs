module Main where

import Prelude
import Data.Maybe
import Data.Traversable
import ChartJs
import WebSocket
import qualified Control.Monad.Eff.Console as C
import Control.Monad.Eff (Eff ())
import Control.Monad.Eff.Exception
import Control.Monad.Trans (lift)
import qualified Control.Monad.Eff.JQuery as J
import Data.Either (Either (Left, Right))
import DOM
import Graphics.Canvas

--import Debug.Trace (trace)

main :: forall eff. Eff (ws :: WS, console :: C.CONSOLE, dom :: DOM, canvas::Canvas, err :: EXCEPTION | eff) Unit
main = do
  C.log "START"
  runWebSocket $ do
    result <- withWebSocket config handlers
    case result of
         Right _ -> output "DONE"
         Left err -> output err

  void <<< J.ready $ do
    initGraph "bar" initBar

config =
  { uri: "ws://127.0.0.1:9001"
  , protocols: []
  }

handlers :: forall eff. WebSocketHandler (console :: C.CONSOLE | eff)
handlers s = (defaultHandlers s)
  { onOpen = onOpenHandler
  , onMessage = onMessageHandler
  }

onOpenHandler :: forall eff. WithWebSocket (console :: C.CONSOLE | eff) Unit
onOpenHandler = C.log "OPEN"

onMessageHandler :: forall eff. String -> WithWebSocket (console :: C.CONSOLE | eff) Unit
onMessageHandler msg = C.log msg

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
   . Chart
  -> Eff ( dom :: DOM , err :: EXCEPTION, canvas:: Canvas | eff ) ChartType
initBar c =
  barChart c barData (responsiveChartConfig defBarChartConfig)
  where
    barData = {
      labels : ["user","nice","system","idle","iowait","irq","softirq"],
      datasets : [
        { fillColor : "rgba(220,220,220,0.5)"
        , strokeColor : "rgba(220,220,220,0.8)"
        , highlightFill: "rgba(220,220,220,0.75)"
        , highlightStroke: "rgba(220,220,220,1)"
        , data : [70.0, 92.0, 89.0, 79.0, 34.0, 64.0, 1.0]
        },
        { fillColor : "rgba(151,187,205,0.5)"
        , strokeColor : "rgba(151,187,205,0.8)"
        , highlightFill : "rgba(151,187,205,0.75)"
        , highlightStroke : "rgba(151,187,205,1)"
        , data : [24.0, 8.0, 62.0, 48.0, 84.0, 45.0, 4.0]
        }
        ]}

die :: forall eff a. String -> Eff( err:: EXCEPTION | eff ) a
die = error >>> throwException
