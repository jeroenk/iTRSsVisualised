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

-- This module defines the state of the visualization program.

module Environment (
    VisiblePos,
    Background(Black, White),
    SymbolColor,
    Environment(Env, envRed, redList, winSize, visLU, visRD, background,
                colors, generator, mouseUse, initPos, curPos, nodeTex,
                symFont),
    EnvironmentRef,
    initEnvironment
) where

import SignatureAndVariables
import RuleAndSystem
import Reduction

import Prelude
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
        envRed     :: CReduction s v r,  -- Reduction being visualized
        redList    :: Maybe DisplayList, -- Rendered version of reduction
        -- Window and display
        winSize    :: Size,              -- Physical window size
        visLU      :: VisiblePos,        -- (left, up) of visible area
        visRD      :: VisiblePos,        -- (right, down) of visible area
        -- Colors
        background :: Background,        -- Background colors
        colors     :: [SymbolColor s v], -- Symbol to color mapping
        generator  :: StdGen,            -- Rand. number generator for colors
        -- Mouse zoom
        mouseUse   :: Bool,              -- Box selection active
        initPos    :: Position,          -- (left, up) box
        curPos     :: Position,          -- (right, down) box
        -- Textures
        nodeTex    :: TextureObject,     -- Node texture
        symFont    :: Font               -- Font texture
      }

type EnvironmentRef s v r = IORef (Environment s v r)

-- Initialize the environment
initEnvironment :: RewriteSystem s v r
    => CReduction s v r -> FilePath -> FilePath -> Int -> Size -> GLdouble
       -> GLdouble -> IO (IORef (Environment s v r))
initEnvironment red n_file f_file f_scale w_size vis_width vis_height = do
    -- Initialize node texture
    node <- loadImageTexture n_file

    -- Initialize font
    font <- loadFontTexture f_file
    _ <- setFontFaceSize font (24 * f_scale) 72

    -- Initialize random number generator
    gen <- newStdGen

    -- Initialize environment
    newIORef Env {
        envRed     = red,
        redList    = Nothing,
        winSize    = w_size,
        visLU      = (0.0, 0.0),
        visRD      = (vis_width, vis_height),
        background = Black,
        colors     = [],
        generator  = gen,
        mouseUse   = False,
        initPos    = Position 0 0,
        curPos     = Position 0 0,
        nodeTex    = node,
        symFont    = font
        }
