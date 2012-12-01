{-
Copyright (C) 2011, 2012 Jeroen Ketema

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

module Main (
    main
) where

import Prelude hiding (catch)
import Control.Exception
import Control.Monad
import Data.Maybe
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import System.FilePath

import DrawReduction
import DrawMouseSquare
import Environment
import Utilities

import SignatureAndVariables
import RuleAndSystem

-- Initial window size
initWinSize :: Size
initWinSize = Size 1000 500

-- Texture for the nodes in term trees
nodeFile :: FilePath
nodeFile = "node.png"

-- Font for the node labels
fontFile :: FilePath
fontFile = "fonts" ++ pathSeparator:"FreeSans.ttf"

main :: IO ()
main = do
    -- Initialize reduction
    (program_name, args) <- getArgsAndInitialize

    reduction_file <- case args of
        [fn] -> return (if   drop (length fn - 3) fn == ".hs"
                        then take (length fn - 3) fn
                        else fn)
        _    -> error $ "usage: " ++ program_name ++ " <reduction>"

    red <- loadReduction reduction_file

    -- Initialize window
    initialDisplayMode $= [DoubleBuffered, RGBAMode, WithDepthBuffer]
    initialWindowSize  $= initWinSize
    _ <- createWindow program_name

    -- Initialize projections
    clearColor $= Color4 0.0 0.0 0.0 1.0
    depthFunc  $= Just Less
    matrixMode $= Projection
    loadIdentity
    ortho 0.0 visualWidth visualHeight 0.0 (-1.0) 1.0
    matrixMode $= Modelview 0
    loadIdentity

    -- Initialize blending
    blendFunc       $= (SrcAlpha, OneMinusSrcAlpha)
    blend           $= Enabled
    lineSmooth      $= Enabled
    hint LineSmooth $= Nicest

    environment <- initEnvironment red nodeFile fontFile fontScale
                                       initWinSize visualWidth visualHeight

    -- Initialize callbacks
    displayCallback       $= display environment
    reshapeCallback       $= Just (reshape environment)
    keyboardMouseCallback $= Just (keyboardMouse environment)
    motionCallback        $= Just (motion environment)

    -- Menu for choosing colors
    let menu = Menu [MenuEntry "Black Background" (blackBackground environment),
                     MenuEntry "White Background" (whiteBackground environment)]
    attachMenu RightButton menu

    -- Main loop
    mainLoop

-- Helper function for screen update after zooming and color change.
updateView :: RewriteSystem s v r
    => EnvironmentRef s v r -> IO ()
updateView environment = do
    env <- get environment
    let (l, u) = visLU env
        (r, d) = visRD env
    -- Delete reduction rendering that might have been for different zoom.
    when (isJust $ redList env) $ do
        deleteObjectNames [fromJust (redList env)]
        environment $= env {redList = Nothing}
    -- Update projection
    matrixMode $= Projection
    loadIdentity
    ortho l r d u (-1.0) 1.0
    matrixMode $= Modelview 0
    loadIdentity
    postRedisplay Nothing

-- Reshape window callback
reshape :: (Signature s, Variables v, RewriteSystem s v r)
    => EnvironmentRef s v r -> ReshapeCallback
reshape environment (Size w h) = do
    env <- get environment
    environment $= env {winSize = Size w' h'}
    viewport    $= (Position 0 0, Size w' h')
    windowSize  $= Size w' h'
    postRedisplay Nothing
        where w' = if h * 2 > w then w else h * 2
              h' = if h * 2 > w then w `div` 2 else h

-- Factor used when moving with cursor keys
moveFac :: GLdouble
moveFac = 0.05

-- Factor used when zooming with + and - keys
zoomFac :: GLdouble
zoomFac = 0.05

-- Keyboard and mouse callback
keyboardMouse :: RewriteSystem s v r
    => EnvironmentRef s v r -> KeyboardMouseCallback
keyboardMouse environment (MouseButton LeftButton) Down _ pos = do
    env <- get environment
    environment $= env {
        mouseUse = True,
        initPos  = pos,
        curPos   = pos
        }
    updateView environment -- Using only "postRedisplay" has performance issues
keyboardMouse environment (MouseButton LeftButton) Up _ _ = do
    env <- get environment
    when (mouseUse env) $ do -- Linux registers left up when in context menu
        let phys_pos = (initPos env, curPos env)
            vis      = (visLU env, visRD env)
            (x, y, x', y') = zoomPosition phys_pos vis (winSize env)
        environment $= env {
            mouseUse = False,
            visLU = (min x x', min y y'),
            visRD = (max x x', max y y')
            }
        updateView environment
keyboardMouse environment (SpecialKey KeyRight) Down _ _ = do
    env <- get environment
    let (x,  y)  = visLU env
        (x', y') = visRD env
        move     = (x' - x) * moveFac
    environment $= env {
        visLU = (x + move, y),
        visRD = (x' + move, y')
        }
    updateView environment
keyboardMouse environment (SpecialKey KeyLeft) Down _ _ = do
    env <- get environment
    let (x,  y)  = visLU env
        (x', y') = visRD env
        move     = (x' - x) * moveFac
    environment $= env {
        visLU = (x - move, y),
        visRD = (x' - move, y')
        }
    updateView environment
keyboardMouse environment (SpecialKey KeyUp) Down _ _ = do
    env <- get environment
    let (x,  y)  = visLU env
        (x', y') = visRD env
        move     = (y' - y) * moveFac
    environment $= env {
        visLU = (x, y - move),
        visRD = (x', y' - move)
        }
    updateView environment
keyboardMouse environment (SpecialKey KeyDown) Down _ _ = do
    env <- get environment
    let (x,  y)  = visLU env
        (x', y') = visRD env
        move     = (y' - y) * moveFac
    environment $= env {
        visLU = (x, y + move),
        visRD = (x', y' + move)
        }
    updateView environment
keyboardMouse environment (Char '+') Down _ _ = do
    env <- get environment
    let (x,  y)  = visLU env
        (x', y') = visRD env
        x_diff = if zoomOk x x' then (x' - x) * zoomFac else 0.0
        y_diff = if zoomOk x x' then (y' - y) * zoomFac else 0.0
    environment $= env {
        visLU = (x  + x_diff, y  + y_diff),
        visRD = (x' - x_diff, y' - y_diff)
        }
    updateView environment
keyboardMouse environment (Char '-') Down _ _ = do
    env <- get environment
    let (x,  y)  = visLU env
        (x', y') = visRD env
        x_diff = (x' - x) * zoomFac
        y_diff = (y' - y) * zoomFac
    environment $= env {
        visLU = (x  - x_diff, y  - y_diff),
        visRD = (x' + x_diff, y' + y_diff)
        }
    updateView environment
keyboardMouse environment (Char 'r') Down _ _ = do
    env <- get environment
    environment $= env {
        visLU = (0.0, 0.0),
        visRD = (visualWidth, visualHeight)
        }
    updateView environment
keyboardMouse _ _ _ _ _ =
    return ()

-- callback for mouse movement
motion :: RewriteSystem s v r
    => EnvironmentRef s v r -> MotionCallback
motion environment (Position x y) = do
    env <- get environment
    let mouse                  = mouseUse env
        (Size w h)             = winSize env
        (Position x_int y_int) = initPos env
    when mouse $ environment $= env {curPos = pos x_int (x' w) y_int (y' h)}
    postRedisplay Nothing
        where pos x_int x_cur y_int y_cur = Position x_new y_new
                  where x_new
                            | x_cur >= x_int = x_int + w_new
                            | otherwise      = x_int - w_new
                        y_new
                            | y_cur >= y_int = y_int + h_new
                            | otherwise      = y_int - h_new
                        w_new = if h' * 2 > w' then w' else h' * 2
                        h_new = if h' * 2 > w' then w' `div` 2 else h'
                        w' = abs (x_cur - x_int)
                        h' = abs (y_cur - y_int)
              x' w = max 0 (min x w)
              y' h = max 0 (min y h)

-- Callbacks for changing the background color.
blackBackground :: RewriteSystem s v r
    => EnvironmentRef s v r -> MenuCallback
blackBackground environment = do
    clearColor $= Color4 0.0 0.0 0.0 1.0
    env <- get environment
    environment $= env {background = Black}
    updateView environment

whiteBackground :: RewriteSystem s v r
    => EnvironmentRef s v r -> MenuCallback
whiteBackground environment = do
    clearColor $= Color4 1.0 1.0 1.0 1.0
    env <- get environment
    environment $= env {background = White}
    updateView environment

-- Callback for displaying (and a number of helper functions).
displayReduction :: (Show s, Show v, RewriteSystem s v r)
    => EnvironmentRef s v r -> IO ()
displayReduction environment = do
    env <- get environment
    when (isNothing $ redList env) $ displayReduction' environment
    env' <- get environment -- Environment updated by displayReduction'
    callList $ fromJust (redList env')

displayReduction' :: (Show s, Show v, RewriteSystem s v r)
    => EnvironmentRef s v r -> IO ()
displayReduction' environment = do
    list <- defineNewList Compile $ drawReduction environment
    env <- get environment
    environment $= env {redList = Just list}

displayMouseSquare :: RewriteSystem s v r
    => EnvironmentRef s v r -> IO ()
displayMouseSquare environment = do
    env <- get environment
    let phys_pos = (initPos env, curPos env)
        vis      = (visLU env, visRD env)
    drawMouseSquare (mouseUse env) phys_pos vis (winSize env) (background env)

-- In case of an error we exit, as we might end up in an infinite loop
displayError :: ErrorCall -> IO ()
displayError err = error $ show err

display :: (Show s, Show v, RewriteSystem s v r)
    => EnvironmentRef s v r -> DisplayCallback
display environment = do
    catch (do clear [ColorBuffer, DepthBuffer]
              displayReduction environment
              displayMouseSquare environment)
        displayError
    flush
    swapBuffers
