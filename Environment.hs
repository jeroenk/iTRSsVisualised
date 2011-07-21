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
    EnvironmentRef,
    init_environment
) where

import SignatureAndVariables
import RuleAndSystem
import Reduction

import Data.IORef
import Graphics.Rendering.OpenGL
import Graphics.Rendering.FTGL
import System.Random

import Utilities

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

-- Initialize the environment
init_environment :: RewriteSystem s v r
    => CReduction s v r -> FilePath -> FilePath -> Int -> Size -> GLdouble
       -> GLdouble -> IO (IORef (Environment s v r))
init_environment red n_file f_file f_scale w_size vis_width vis_height = do
    -- Initialize node texture
    node <- loadImageTexture n_file

    -- Initialize font
    font <- loadFontTexture f_file
    _ <- setFontFaceSize font (24 * f_scale) 72

    -- Initialize random number generator
    gen <- newStdGen

    -- Initialize environment
    newIORef $ Env {
        env_red    = red,
        red_list   = Nothing,
        win_size   = w_size,
        vis_lu     = (0.0, 0.0),
        vis_rd     = (vis_width, vis_height),
        background = Black,
        colors     = [],
        generator  = gen,
        mouse_use  = False,
        init_pos   = Position 0 0,
        cur_pos    = Position 0 0,
        node_tex   = node,
        sym_font   = font
        }
