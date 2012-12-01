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

-- This module implements the drawing of the mouse square used for zooming

module DrawMouseSquare (
    zoomOk,
    zoomPosition,
    drawMouseSquare
) where

import Environment
import DrawReduction

import Prelude
import Graphics.Rendering.OpenGL

-- Maximum allowed zoom.
--
-- Limiting the zoom avoids drawing errors caused by rounding to be shown.
maxZoom :: GLdouble
maxZoom = 1.6e-5

-- Establish if we are allowed to zoom any further.
zoomOk :: GLdouble -> GLdouble -> Bool
zoomOk x x' = abs (x' - x) >=  visualWidth * maxZoom

-- If the zoom area is too small, enlarge to the minimal allowed area.
limitPosition :: GLdouble -> GLdouble -> GLdouble -> GLdouble
    -> (GLdouble, GLdouble, GLdouble, GLdouble)
limitPosition x y x' y'
    | zoomOk x x' = (x, y, x', y')
    | otherwise   = (x, y, x_new', y_new')
    where x_new' = x + visualWidth * (if x' < x then -maxZoom else maxZoom)
          y_new' = y + visualHeight * (if y' < y then -maxZoom else maxZoom)

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
zoomPosition :: (Position, Position) -> (VisiblePos, VisiblePos)
    -> Size -> (GLdouble, GLdouble, GLdouble, GLdouble)
zoomPosition phys_pos vis size = zoomPosition' phys_pos vis size True

zoomPosition' :: (Position, Position) -> (VisiblePos, VisiblePos)
     -> Size -> Bool -> (GLdouble, GLdouble, GLdouble, GLdouble)
zoomPosition' (Position x y, Position x' y') ((l, u), (r, d)) (Size p q) z
    | x == x' && y == y && z = (l, u, r, d)
    | x == x' && y == y = (x_new, y_new, x_new', y_new')
    | otherwise = limitPosition x_new y_new x_new' y_new'
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
drawMouseSquare True phys_pos vis size back =
    unsafePreservingMatrix $ do
        color $ case back of -- Arbitrarily chosen colors for the square
            Black -> Color4 (255.0 * 0.45 :: GLdouble) (255.0 * 0.95) 0.0 1.0
            White -> Color4 0.0 (255.0 * 0.45 :: GLdouble) (255.0 * 0.95) 1.0
        let (x, y, x', y') = zoomPosition' phys_pos vis size False
        renderPrimitive LineLoop $ do
            vertex $ Vertex3 x  y  0.5
            vertex $ Vertex3 x  y' 0.5
            vertex $ Vertex3 x' y' 0.5
            vertex $ Vertex3 x' y  0.5
drawMouseSquare False _ _ _ _ =
    return ()
