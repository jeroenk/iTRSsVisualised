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

-- This module defines the state of the visualization program.

module Environment (
    VisiblePos,
    Background(Black, White),
    SymbolColor,
    Environment(Env, env_red, red_list, win_size, vis_lu, vis_rd, background,
                colors, generator, mouse_use, init_pos, cur_pos, node_tex,
                sym_font),
    EnvironmentRef
) where

import SignatureAndVariables
import RuleAndSystem
import Reduction

import Data.IORef
import Graphics.Rendering.OpenGL
import Graphics.Rendering.FTGL
import System.Random

-- Data type for visible positions
type VisiblePos = (GLdouble, GLdouble)

-- Data type for remembering the background color.
data Background
    = Black
    | White

instance Eq Background where
    Black == Black = True
    White == White = True
    _ == _         = False

-- Mapping from symbols to colors. The first color represents the one used
-- on a black background and the second represents the one used on a white
-- background.
type SymbolColor s v = (Symbol s v, Color4 GLfloat, Color4 GLfloat)

-- Actual state.
data RewriteSystem s v r => Environment s v r
    = Env {
        -- Reduction
        env_red    :: CReduction s v r,  -- Reduction being visualized
        red_list   :: Maybe DisplayList, -- Rendered version of reduction
        -- Window and display
        win_size   :: Size,              -- Physical window size
        vis_lu     :: VisiblePos,        -- (left, up) of visible area
        vis_rd     :: VisiblePos,        -- (right, down) of visible area
        -- Colors
        background :: Background,        -- Background colors
        colors     :: [SymbolColor s v], -- Symbol to color mapping
        generator  :: StdGen,            -- Rand. number generator for colors
        -- Mouse zoom
        mouse_use  :: Bool,              -- Box selection active
        init_pos   :: Position,          -- (left, up) box
        cur_pos    :: Position,          -- (right, down) box
        -- Textures
        node_tex   :: TextureObject,     -- Node texture
        sym_font   :: Font               -- Font texture
      }

type EnvironmentRef s v r = IORef (Environment s v r)
