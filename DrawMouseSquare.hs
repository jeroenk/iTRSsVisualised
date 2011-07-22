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

-- This module implements the drawing of the mouse square used for zooming

module DrawMouseSquare (
    zoom_ok,
    zoom_position,
    drawMouseSquare
) where

import Environment
import DrawReduction

import Graphics.Rendering.OpenGL

-- Maximum allowed zoom
--
-- Limiting the zoom avoids drawing errors caused by rounding to be shown.
max_zoom :: GLdouble
max_zoom = 1.6e-5

-- Establish if we are allowed to zoom any further.
zoom_ok :: GLdouble -> GLdouble -> Bool
zoom_ok x x' = abs (x' - x) >=  visual_width * max_zoom

-- If the zoom area is too small, enlarge to the minimal allowed area.
limit_position :: GLdouble -> GLdouble -> GLdouble -> GLdouble
    -> (GLdouble, GLdouble, GLdouble, GLdouble)
limit_position x y x' y'
    | zoom_ok x x' = (x, y, x', y')
    | otherwise    = (x, y, x_new', y_new')
    where x_new' = x + visual_width * (if x' < x then -max_zoom else max_zoom)
          y_new' = y + visual_height * (if y' < y then -max_zoom else max_zoom)

-- Calculate the area to zoom to in terms of the drawing area of the reduction.
--
-- The parameters are as follows:
-- * phys_pos: physical screen coordinates of the mouse
-- * vis:      currently visible drawing area
-- * size:     current window size
--
-- The additional zoom parameter of zoom_position' indicates if the calculated
-- positions are either for an actual zoom or just to draw the mouse square.
-- The behavior of the tow is different in case the mouse has not moved from
-- its initial position.
zoom_position :: (Position, Position) -> (VisiblePos, VisiblePos)
    -> Size -> (GLdouble, GLdouble, GLdouble, GLdouble)
zoom_position phys_pos vis size = zoom_position' phys_pos vis size True

zoom_position' :: (Position, Position) -> (VisiblePos, VisiblePos)
     -> Size -> Bool -> (GLdouble, GLdouble, GLdouble, GLdouble)
zoom_position' (Position x y, Position x' y') ((l, u), (r, d)) (Size p q) z
    | x == x' && y == y && z = (l, u, r, d)
    | x == x' && y == y = (x_new, y_new, x_new', y_new')
    | otherwise = limit_position x_new y_new x_new' y_new'
    where x_new   = l + fromIntegral x  * x_scale  :: GLdouble
          y_new   = u + fromIntegral y  * y_scale  :: GLdouble
          x_new'  = l + fromIntegral x' * x_scale :: GLdouble
          y_new'  = u + fromIntegral y' * y_scale :: GLdouble
          x_scale = (r - l) / fromIntegral p
          y_scale = (d - u) / fromIntegral q

-- drawMouseSquare
--
-- The first parameter indicates if a mouse square needs to be drawn. The
-- last parameter indicates the background color. The other parameters
-- correspond to those of zoom_position.
drawMouseSquare :: Bool -> (Position, Position) -> (VisiblePos, VisiblePos)
    -> Size -> Background -> IO ()
drawMouseSquare True phys_pos vis size back = do
    unsafePreservingMatrix $ do
        color $ case back of -- Arbitrarily chosen colors for the square
            Black -> Color4 (255.0 * 0.45 :: GLdouble) (255.0 * 0.95) 0.0 1.0
            White -> Color4 0.0 (255.0 * 0.45 :: GLdouble) (255.0 * 0.95) 1.0
        let (x, y, x', y') = zoom_position' phys_pos vis size False
        renderPrimitive LineLoop $ do
            vertex $ Vertex3 x  y  0.5
            vertex $ Vertex3 x  y' 0.5
            vertex $ Vertex3 x' y' 0.5
            vertex $ Vertex3 x' y  0.5
drawMouseSquare False _ _ _ _ = do
    return ()
