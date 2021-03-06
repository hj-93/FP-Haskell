-- This module is a starting point for implementing the Graph Drawing
-- Calculator as described in Part II of the Standard Lab. You can use this
-- directly, or just study it as an example of how to use threepenny-gui.

import ThreepennyPages
import Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny as UI
import Data.Maybe
import Expr (eval, readExpr, Expr, differentiate)
canWidth,canHeight :: Num a => a
canWidth  = 300
canHeight = 300

main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup window =
  do -- Create them user interface elements
     canvas  <- mkCanvas canWidth canHeight   -- The drawing area
     fx      <- mkHTML "<i>f</i>(<i>x</i>)="  -- The text "f(x)="
     input   <- mkInput 20 "x"                -- The formula input
     draw    <- mkButton "Draw graph"         -- The draw button
     zoom    <- mkSlider (1, 100) 50          -- The zoom IN/OUT slider
     diff    <- mkButton "Draw Differentiate" -- The diff button
       -- The markup "<i>...</i>" means that the text inside should be rendered
       -- in italics.

     -- Add the user interface elements to the page, creating a specific layout
     formula <- row [pure fx,pure input]
     getBody window #+ [column [pure canvas,pure formula,pure draw, pure zoom, pure diff]]

     -- Styling
     getBody window # set style [("backgroundColor","lightblue"),
                                 ("textAlign","center")]
     pure input # set style [("fontSize","14pt")]

     -- Interaction (install event handlers)
     on UI.click     draw  $ \ _ -> readAndDraw input zoom False canvas
     on UI.click     diff  $ \ _ -> readAndDraw input zoom True canvas

     on valueChange' input $ \ _ -> readAndDraw input zoom False canvas
     on valueChange' zoom  $ \ _ -> readAndDraw input zoom False canvas

readAndDraw :: Element -> Element -> Bool -> Canvas -> UI ()
readAndDraw input zoom diff canvas =
  do
      -- Get the current zoom (a String) from the zoom element
     scale <- ((/1000) . read) <$> get value zoom

     -- Get the current formula (a String) from the input element
     maybeExp <- readExpr <$> get value input
     let maybeExp' = case diff of
                          True  -> differentiate <$> maybeExp
                          False -> maybeExp
     let allpoints = case maybeExp' of
                       Nothing  -> []
                       Just exp -> points exp scale (canWidth, canHeight)

     -- Clear the canvas
     clearCanvas canvas

     -- The following code draws the formula text in the canvas and a blue line.
     -- It should be replaced with code that draws the graph of the function.
     set UI.fillStyle (UI.solidColor (UI.RGB 0 0 0)) (pure canvas)
     path "blue" allpoints canvas

-- threepenny type Point = (Double, Double)

-- calculates all the points of the graph in terms of pixels
points :: Expr -> Double -> (Int,Int) -> [Point]
points exp scale (width, height) = zip xs_pix ys_pix
  where
        -- converts a pixel x-coordinate to a real x-coordinate
        pixToReal :: Double -> Double
        pixToReal x = scale * (x - fromIntegral width / 2)

        -- converts a real y-coordinate to a pixel y-coordinate
        realToPix :: Double -> Double
        realToPix y =  -y / scale + (fromIntegral height) / 2

        xs_pix  = map fromIntegral [0..width]
        xs_real = map pixToReal xs_pix
        ys_pix  = map (realToPix . eval exp) xs_real
