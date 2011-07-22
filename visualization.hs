{-
Copyright (C) 2011 Jeroen Ketema

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

import qualified Control.Exception as E
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
init_win_size :: Size
init_win_size = Size 1000 500

-- Texture for the nodes in term trees
node_file :: FilePath
node_file = "node.png"

-- Font for the node labels
font_file :: FilePath
font_file = "fonts" ++ pathSeparator:"FreeSans.ttf"

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
    initialWindowSize  $= init_win_size
    _ <- createWindow program_name

    -- Initialize projections
    clearColor $= Color4 0.0 0.0 0.0 1.0
    depthFunc  $= Just Less
    matrixMode $= Projection
    loadIdentity
    ortho 0.0 visual_width visual_height 0.0 (-1.0) 1.0
    matrixMode $= Modelview 0
    loadIdentity

    -- Initialize blending
    blendFunc       $= (SrcAlpha, OneMinusSrcAlpha)
    blend           $= Enabled
    lineSmooth      $= Enabled
    hint LineSmooth $= Nicest

    environment <- init_environment red node_file font_file font_scale
                                        init_win_size visual_width visual_height

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
update_view :: RewriteSystem s v r
    => EnvironmentRef s v r -> IO ()
update_view environment = do
    env <- get environment
    let (l, u) = vis_lu env
        (r, d) = vis_rd env
    -- Delete reduction rendering that might have been for different zoom.
    when (isJust $ red_list env) $ do
        deleteObjectNames [fromJust (red_list env)]
        environment $= env {red_list = Nothing}
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
    environment $= env {win_size = Size w' h'}
    viewport    $= (Position 0 0, Size w' h')
    windowSize  $= Size w' h'
    postRedisplay Nothing
        where w' = if h * 2 > w then w else h * 2
              h' = if h * 2 > w then w `div` 2 else h

-- Factor used when moving with cursor keys
move_fac :: GLdouble
move_fac = 0.05

-- Factor used when zooming with + and - keys
zoom_fac :: GLdouble
zoom_fac = 0.05

-- Keyboard and mouse callback
keyboardMouse :: RewriteSystem s v r
    => EnvironmentRef s v r -> KeyboardMouseCallback
keyboardMouse environment (MouseButton LeftButton) Down _ pos = do
    env <- get environment
    environment $= env {
        mouse_use = True,
        init_pos  = pos,
        cur_pos   = pos
        }
    update_view environment -- Using only "postRedisplay" has performance issues
keyboardMouse environment (MouseButton LeftButton) Up _ _ = do
    env <- get environment
    when (mouse_use env) $ do -- Linux registers left up when in context menu
        let phys_pos = (init_pos env, cur_pos env)
            vis      = (vis_lu env, vis_rd env)
            (x, y, x', y') = zoom_position phys_pos vis (win_size env)
        environment $= env {
            mouse_use = False,
            vis_lu = (min x x', min y y'),
            vis_rd = (max x x', max y y')
            }
        update_view environment
keyboardMouse environment (SpecialKey KeyRight) Down _ _ = do
    env <- get environment
    let (x,  y)  = vis_lu env
        (x', y') = vis_rd env
        move     = (x' - x) * move_fac
    environment $= env {
        vis_lu = (x + move, y),
        vis_rd = (x' + move, y')
        }
    update_view environment
keyboardMouse environment (SpecialKey KeyLeft) Down _ _ = do
    env <- get environment
    let (x,  y)  = vis_lu env
        (x', y') = vis_rd env
        move     = (x' - x) * move_fac
    environment $= env {
        vis_lu = (x - move, y),
        vis_rd = (x' - move, y')
        }
    update_view environment
keyboardMouse environment (SpecialKey KeyUp) Down _ _ = do
    env <- get environment
    let (x,  y)  = vis_lu env
        (x', y') = vis_rd env
        move     = (y' - y) * move_fac
    environment $= env {
        vis_lu = (x, y - move),
        vis_rd = (x', y' - move)
        }
    update_view environment
keyboardMouse environment (SpecialKey KeyDown) Down _ _ = do
    env <- get environment
    let (x,  y)  = vis_lu env
        (x', y') = vis_rd env
        move     = (y' - y) * move_fac
    environment $= env {
        vis_lu = (x, y + move),
        vis_rd = (x', y' + move)
        }
    update_view environment
keyboardMouse environment (Char '+') Down _ _ = do
    env <- get environment
    let (x,  y)  = vis_lu env
        (x', y') = vis_rd env
        x_diff = if zoom_ok x x' then (x' - x) * zoom_fac else 0.0
        y_diff = if zoom_ok x x' then (y' - y) * zoom_fac else 0.0
    environment $= env {
        vis_lu = (x  + x_diff, y  + y_diff),
        vis_rd = (x' - x_diff, y' - y_diff)
        }
    update_view environment
keyboardMouse environment (Char '-') Down _ _ = do
    env <- get environment
    let (x,  y)  = vis_lu env
        (x', y') = vis_rd env
        x_diff = (x' - x) * zoom_fac
        y_diff = (y' - y) * zoom_fac
    environment $= env {
        vis_lu = (x  - x_diff, y  - y_diff),
        vis_rd = (x' + x_diff, y' + y_diff)
        }
    update_view environment
keyboardMouse environment (Char 'r') Down _ _ = do
    env <- get environment
    environment $= env {
        vis_lu = (0.0, 0.0),
        vis_rd = (visual_width, visual_height)
        }
    update_view environment
keyboardMouse _ _ _ _ _ = do
    return ()

-- callback for mouse movement
motion :: RewriteSystem s v r
    => EnvironmentRef s v r -> MotionCallback
motion environment (Position x y) = do
    env <- get environment
    let mouse                  = mouse_use env
        (Size w h)             = win_size env
        (Position x_int y_int) = init_pos env
    when mouse $ environment $= env {cur_pos = pos x_int (x' w) y_int (y' h)}
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
    update_view environment

whiteBackground :: RewriteSystem s v r
    => EnvironmentRef s v r -> MenuCallback
whiteBackground environment = do
    clearColor $= Color4 1.0 1.0 1.0 1.0
    env <- get environment
    environment $= env {background = White}
    update_view environment

-- Callback for displaying (and a number of helper functions).
displayReduction :: (Show s, Show v, RewriteSystem s v r)
    => EnvironmentRef s v r -> IO ()
displayReduction environment = do
    env <- get environment
    when (isNothing $ red_list env) $ displayReduction' environment
    env' <- get environment -- Environment updated by displayReduction'
    callList $ fromJust (red_list env')

displayReduction' :: (Show s, Show v, RewriteSystem s v r)
    => EnvironmentRef s v r -> IO ()
displayReduction' environment = do
    list <- defineNewList Compile $ drawReduction environment
    env <- get environment
    environment $= env {red_list = Just list}

displayMouseSquare :: RewriteSystem s v r
    => EnvironmentRef s v r -> IO ()
displayMouseSquare environment = do
    env <- get environment
    let phys_pos = (init_pos env, cur_pos env)
        vis      = (vis_lu env, vis_rd env)
    drawMouseSquare (mouse_use env) phys_pos vis (win_size env) (background env)

-- In case of an error we exit, as we might end up in an infinite loop
displayError :: E.ErrorCall -> IO ()
displayError err = error $ show err

display :: (Show s, Show v, RewriteSystem s v r)
    => EnvironmentRef s v r -> DisplayCallback
display environment = do
    E.catch (do clear [ColorBuffer, DepthBuffer]
                displayReduction environment
                displayMouseSquare environment)
        displayError
    flush
    swapBuffers
