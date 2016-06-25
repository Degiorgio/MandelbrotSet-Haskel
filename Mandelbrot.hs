{-----------------------------------------------------------------
==================================================================
MAJOR PROGRAM - Mandlebrotset 
AUTHOR: Kurt.Degiorgio 
==================================================================
Implemented using the SDL bindings for Haskell.

Intructions: execute the main function and use the following 
controls to manipulate the image.

Mouse Left-Click: Zooms in the click area centered on click.
Mouse Right-Click: Zooms out the click area centered on click.
Keyboard SPACE key: Restore image to original state.
Keyboard LEFT-ARROW: Increments iteration cout.
Keyboard RIGHT-ARROW: Decrments iteration count.
Keyboard TOP-ARROW: iterates through color schemes.
-----------------------------------------------------------------}

-- Needed for the colors
import Data.Word
-- Needed for Complex Numbers
import Data.Complex
-- Needed for Return
import Control.Monad   
-- Needed for the SDL grahics to 
-- draw the Mandelbrot image
import Graphics.UI.SDL 

-- Defined rectangle being used for zooming purposes
-- Four doubles that depict the rectangle.
type MandelRectangle = (Double, Double, Double, Double)  

-- Represents a single pixel on the mandelbrot set.
type MandelPixel  = (Int,Int)

-- Represents a Color in the RGB color space.
type RGBAColor   = (Word8,Word8,Word8)

-- Represents a UI Screen
type Scene  = Graphics.UI.SDL.Surface

-- Image size implemented as a tuple 
-- where the first element is the width and the second element is the height.
dimensions :: (Int, Int)
dimensions = (450, 450)

-- The Max iteration count determines  how long it will take for
-- a point to  exit the mandelbrot set.
-- Can be changed by the user dynamically.
maxIterations :: Int
maxIterations = 100

-- Bound Rectangle
-- Defualt ViewPoint of the Mandelbrot set.
-- This View-Point will be manipulated to achieve the zooming effect.
mandelViewPoint :: MandelRectangle  
mandelViewPoint = ( -2, -1.2, 2.6, 2.4)

---------------------------------------------------------------------------------------
-- MandelBrot Computational Functions
---------------------------------------------------------------------------------------

-- Conduct the Mandelbrot Computation.
------------------------------------------------------------
-- Parameter 1: Complex number Z
-- Parameter 2: Complex number C
-- Output: Computed Complex number
computerSinglePoint :: (RealFloat a) => Complex a -> Complex a -> Complex a
computerSinglePoint z c = (z*z)+c

-- Recursively Compute the Mandelbrot set
------------------------------------------------------------
-- Parameter 1: Complex number for which we shall iteratate
-- Parameter 2: Complex number for which we shall iterate
-- Parameter 3: Max iteration count
-- Output: required iterations
computeIterations :: (Eq a, Num a, RealFloat a1) => Complex a1 -> Complex a1 -> a -> a
computeIterations c z 0 = 0
computeIterations c z maxIter   
    | realComplex(z) > 4 = maxIter  
    | otherwise          = computeIterations c (computerSinglePoint z c) (maxIter-1)

-- Calculates the scalar factor based on the canvas size.
------------------------------------------------------------
-- Parameter 1: Position
-- Parameter 2: Gradeint
-- Parameter 3: y Intercept
calculateScalar :: (Fractional a, Integral a1) => a1 -> a -> a -> a
calculateScalar m x b = (((fromIntegral m) / (fromIntegral (fst dimensions))) * x) + b

-- Convert a Tuple to a complex number.
castToComplex :: (a, a) -> Complex a
castToComplex (r,i) = (r:+i) 

-- Get the Real Part of the Complex number.
realComplex :: (RealFloat a) => Complex a -> a
realComplex(r:+_) = r

---------------------------------------------------------------------------------------		    
-- Image Generation Functions
---------------------------------------------------------------------------------------

-- Generates a list of pixel based on the height and width of the image.
-- The first element of the tuple represeant points on the X axis.
-- The scond element of the tuple represeant points on the y axis.
------------------------------------------------------------
-- Output: List of  Pixels
generatePixelList :: [MandelPixel]
generatePixelList = [(x,y) | x <- [0..(fst dimensions-1)], y <- [0..(snd dimensions-1)] ] 

-- Generate Images if and only if restore determint is 1
------------------------------------------------------------
-- Parameter 1: Screen to generate image on
-- Parameter 2: List of pixels
-- Parameter 3: Zoom rectangle (subset of set that will be generated)
-- Parameter 4: current Max iteration count
-- Parameter 5: current Color Scheme Index
-- Parameter 6: Action Determint
-- Output: Action List
generateImage :: Scene -> [MandelPixel] -> MandelRectangle -> Int -> Int -> Int -> IO ()
generateImage scr pix viewPoint iter schemeIndex 0 = putStrLn "Waiting For event."
generateImage scr pix viewPoint iter schemeIndex 1 = sdlranderMultiplePixels scr pixelcolors
    where 
      pixelcolors = map (processPixel iter viewPoint schemeIndex) $ pix 

-- Caclulate the color of a single pixel using the Mandlebrot functions
-------------------------------------------------------------
-- Parameter 1: current max Iteration count 
-- Parameter 2: Zoom Rectangle
-- Parameter 3: Pixel to be processed.
-- Parameter 4: current Color Scheme Index
-- Output: Tuple combination of pixel and its color.
processPixel :: Int -> MandelRectangle -> Int -> MandelPixel -> (MandelPixel, RGBAColor)
processPixel cmaxIter viewPoint schemeIndex pixel  = 
  let 
    iteration = computeIterations (castToComplex(scalePixel viewPoint pixel)) 0 cmaxIter
  in 
    (pixel, currentColorSelect iteration  cmaxIter schemeIndex)

-- Caclulate the color of a single pixel using the Mandlebrot functions
-------------------------------------------------------------
-- Parameter 1: Zoom Rectangle
-- Parameter 2: Pixel to be scaled
-- Output: Pixel scaled according to zoom rectangle
scalePixel :: MandelRectangle -> MandelPixel-> (Double, Double)
scalePixel (x, y, width, height) (x1,y1) = (calculateScalar x1 width x, calculateScalar y1 height y)

-- Changes Iteration count
----------------------------------------
-- Parameter 1: Indicates weahter to increment or decrement
-- Parameter 2: current Iteration count.
-- Output: new maximum iteration count.
changeIteration :: Int -> Int -> Int
changeIteration 0 iter =  iter + 10
changeIteration _  10  =  10
changeIteration 1 iter =  iter - 10

---------------------------------------------------------------------------------------e
-- COLORING FUNCTIONS
-- Used to selected pixel based on iteration value
---------------------------------------------------------------------------------------

-- Black Color 
black :: RGBAColor
black = (0,0,0)

-- Red Color Scheme
redScheme :: Int -> RGBAColor
redScheme 0 = (255,0,0)
redScheme x = (255,255,255)

-- Gold Color Scheme
goldScheme :: Int  -> Int -> RGBAColor
goldScheme x y 
    | x > y-1 = black
    | otherwise = (goldColorScheme $ mod x 16)

goldColorScheme :: Int -> RGBAColor
goldColorScheme 0 = (40,30,25)
goldColorScheme 1 = (26,30,25)
goldColorScheme 2 = (43,30,25)
goldColorScheme 3 = (100,7,26)
goldColorScheme 4 = (4,1,83)
goldColorScheme 5 = (50,4,63)
goldColorScheme 6 = (10,7,120)
goldColorScheme 7 = (50,82,177)
goldColorScheme 8 = (43,181,29)
goldColorScheme 9 = (99,236,248)
goldColorScheme 10 = (33,33,191)
goldColorScheme 11 = (25,221,95)
goldColorScheme 12 = (255,43,0)
goldColorScheme 13 = (102,63,0)
goldColorScheme 14 = (92,61,0)
goldColorScheme 15 = (73,92,3)

-- Continous Color Scheme
continiousScheme :: Int -> Int ->  RGBAColor
continiousScheme x maxIter
    | x > maxIter-1 = black
    | otherwise = (bernsteinScheme $  (fromIntegral x) / (fromIntegral maxIter))

-- Uses the Berstein Polynomials to generate a color.
bernsteinScheme :: Double -> RGBAColor
bernsteinScheme x = (r,b,c)
    where r = floor (8 * (1 - x) * x * x * x * 255)
          b = floor (16 * (1 - x) * (1 - x) * x * x * 255)
          c = floor (10 * (1 - x) * (1 - x) * (1 - x) * x * 255)

-- Converts Color scheme index to a higher-order function.
--------------------------------------------------------
-- Parameter 1: Current Iteration
-- Parameter 2: Current Max iteration
-- Parameter 3: Current Color Scheme Index
currentColorSelect :: Int -> Int -> Int -> RGBAColor
currentColorSelect iter maxIter 0 = goldScheme iter maxIter
currentColorSelect iter maxIter 1 = continiousScheme iter maxIter
currentColorSelect iter maxIter 2 = redScheme iter

-- Increment the icolor index by one
-- Check 'currentColorSelect' for meaning of each value
----------------------------------------------------------
-- Parameter 1: Existing Index.
-- Parameter 2: New Index.
incrementColorIndex :: Int -> Int
incrementColorIndex 2 = 0
incrementColorIndex x = x + 1

---------------------------------------------------------------------------------------
-- Main Function
-- Execute  start the progam
-- Unfortunately we have to use the 'do' here pattern as
-- we are simply using a language binding for SDL which
-- is implemented imperatively.
---------------------------------------------------------------------------------------

main :: IO ()
main = do
  Graphics.UI.SDL.init [InitEverything]
  Graphics.UI.SDL.setVideoMode (fst dimensions)  (snd dimensions) 32 [HWSurface]
  screen <- Graphics.UI.SDL.getVideoSurface
  sdlHandler screen 1  mandelViewPoint generatePixelList 1 maxIterations

---------------------------------------------------------------------------------------
-- SDL Functions
-- SDL Specfic functions used to print pixels on the screen
-- and capture mouse input wvent.
---------------------------------------------------------------------------------------

-- Event Handler for SDL (Recursive function)
---------------------------------------------------------------
-- Parameter 1: Current Screen
-- Parameter 2: Restore determint
-- Parameter 3: Zoom Rectangle
-- Parameter 4: List of pixels
-- Parameter 5: Current Color Index
-- Parameter 6: Current Max Iteration
-- Output: Action List
sdlHandler :: Scene -> Int -> MandelRectangle -> [MandelPixel] -> Int -> Int -> IO ()
sdlHandler scr restore viewPoint rawPixels colorIndex iter = do
  generateImage scr rawPixels viewPoint iter colorIndex restore 
  Graphics.UI.SDL.tryFlip scr
  e <- Graphics.UI.SDL.waitEvent
  case e of
      Graphics.UI.SDL.Quit -> Control.Monad.return ()
      Graphics.UI.SDL.MouseButtonDown xpos ypos Graphics.UI.SDL.ButtonLeft  -> sdlZoom 0.1 scr  viewPoint rawPixels (fromIntegral(xpos),fromIntegral(ypos)) colorIndex iter
      Graphics.UI.SDL.MouseButtonDown xpos ypos Graphics.UI.SDL.ButtonRight -> sdlZoom 10  scr  viewPoint rawPixels (fromIntegral(xpos),fromIntegral(ypos)) colorIndex iter
      Graphics.UI.SDL.KeyDown (Keysym SDLK_SPACE _ _) ->  sdlHandler scr 1  mandelViewPoint generatePixelList 1 maxIterations
      Graphics.UI.SDL.KeyDown (Keysym SDLK_UP _ _) ->  sdlChangeColor scr viewPoint rawPixels colorIndex iter
      Graphics.UI.SDL.KeyDown (Keysym SDLK_RIGHT _ _) -> sdlChangeIteration 0 scr viewPoint rawPixels colorIndex iter
      Graphics.UI.SDL.KeyDown (Keysym SDLK_LEFT _ _) -> sdlChangeIteration  1 scr viewPoint rawPixels colorIndex iter
      otherwise -> sdlHandler scr 0  viewPoint rawPixels  colorIndex iter


-- Handles zoom event, will calculated new mandelbrot bounds based on 
-- wheres the user wants to zoom
---------------------------------------------------------------
-- Parameter 1: Zoom Factor
-- Parameter 2: Current Screen 
-- Parameter 3: Zoom Rectnagle
-- Parameter 4: List of pixels
-- Parameter 5: Mouse Position
-- Parameter 6: Current Color Index
-- Parameter 7: Current Iteration count   s zoom viewPoint mousePos)
-- Output:  Action List.
sdlZoom :: Double -> Scene ->  MandelRectangle -> [MandelPixel] -> (Int,Int) -> Int -> Int -> IO ()
sdlZoom  zoom scr (x, y, width, height) rawPixels mousePos colorIndex  iter = do       
  putStrLn "Zoom Detected!"
  sdlHandler scr 1 (newRect) rawPixels colorIndex iter
    where
      newRect = (fst (scalePixel (x,y,width,height) mousePos) - (width*zoom)/2, snd (scalePixel (x,y,width,height) mousePos) - (height*zoom)/2, width*zoom, height*zoom)
     

-- Handles change color event
-- uses Lambda functions to increment color index
---------------------------------------------------------------
-- Parameter 1: Current Screen 
-- Parameter 2: Zoom Rectnagle
-- Parameter 3: List of pixels
-- Parameter 4: Current Color Index
-- Parameter 5: Current Iteration count
-- Output:  Action List.
sdlChangeColor :: Scene -> MandelRectangle -> [MandelPixel] -> Int -> Int -> IO ()
sdlChangeColor  scr viewPoint rawPixels colorIndex maxIter = do     
  putStrLn "Changing Color Scheme";
  sdlHandler scr 1 viewPoint  rawPixels ((\x -> case () of
                 _ | x == 2 -> 0
                   | otherwise -> x+1
                  ) colorIndex) maxIter

-- Handles increment/decrement iteration count
---------------------------------------------------------------
-- Parameter 1: Increment or Decrement idenitifier (+ or - )
-- Parameter 2: Current Screen 
-- Parameter 3: Zoom Rectnagle
-- Parameter 4: List of pixels
-- Parameter 5: Current Color Index
-- Parameter 6: Current Iteration count
-- Output:  Action List.
sdlChangeIteration :: Int -> Scene -> MandelRectangle -> [MandelPixel] -> Int -> Int -> IO ()
sdlChangeIteration sign scr viewPoint rawPixels colorIndex iter = do  
  putStrLn $ "increment Iteration count: " ++ (show iter)
  sdlHandler scr 1 viewPoint  rawPixels colorIndex (changeIteration sign iter)

-- Write single pixel to the screen
-- Does so by creating traingle that is 1 pixel deep and
-- converts the RGBA coler to actual color as supported by SDL.
---------------------------------------------------------------
-- Parameter 1: Surface to Draw pixel on
-- Parameter 2: Pixel to be drawn
-- Parameter 3: Color the pixel
-- Output:  Action List
sdlWritePixel :: Scene -> MandelPixel -> RGBAColor -> IO () 
sdlWritePixel scr (x,y) color = do
  let sdRect = Just (Graphics.UI.SDL.Rect x y 1 1)
  sdColor <- sdlColorConvertor scr color
  Graphics.UI.SDL.fillRect scr sdRect sdColor
  return()

-- Writes Multiple  pixels to the screen
---------------------------------------------------------------
-- Parameter 1: Surface to Draw Pixels on.
-- Parameter 2: List of Pixels with assoicated Color
-- Output: Action List
sdlranderMultiplePixels :: Scene  -> [(MandelPixel,RGBAColor)] -> IO () 
sdlranderMultiplePixels scr pixels =  mapM_ writePixel (pixels) 
  where writePixel ((x,y),color) = sdlWritePixel scr (x,y) color

-- Converts a RGBAColor into an SDL color.
-------------------------------------------------
-- Parameter 1: Pixel Format will be based on screen
-- Parameter 2: Color to be converted
-- Output: SDL Color
sdlColorConvertor :: Scene -> RGBAColor -> IO Pixel
sdlColorConvertor scr (r,g,b) = Graphics.UI.SDL.mapRGB (Graphics.UI.SDL.surfaceGetPixelFormat scr) r g b
