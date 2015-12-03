module ChartLens where

import Data.Lens
import Data.Lens.Setter

import ChartJs

animation :: forall a b r. Lens { animation :: a | r } { animation :: b | r } a b
animation = lens _.animation (_ { animation = _ })

global :: forall a b r. Lens { global :: a | r } { global :: b | r } a b
global = lens _.global (_ { global = _ })

legendTemplate :: forall a b r. Lens { legendTemplate :: a | r } { legendTemplate :: b | r } a b
legendTemplate = lens _.legendTemplate (_ { legendTemplate = _ })

maintainAspectRatio :: forall a b r. Lens { maintainAspectRatio :: a | r } { maintainAspectRatio :: b | r } a b
maintainAspectRatio = lens _.maintainAspectRatio (_ { maintainAspectRatio = _ })

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
