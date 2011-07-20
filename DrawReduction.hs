{-# LANGUAGE DoAndIfThenElse #-}
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

-- This module implements the drawing of reductions.

module DrawReduction (
    drawReduction,
    visual_width,
    visual_height,
    font_scale
) where

import SignatureAndVariables
import Term
import PositionAndSubterm
import RuleAndSystem
import SystemOfNotation
import Reduction

import Data.Array
import Data.List
import Data.Maybe
import Graphics.Rendering.FTGL
import Graphics.Rendering.OpenGL hiding (Position)
import System.Random

import Environment

-- Scaling of fonts applied during the drawing of node labels.
font_scale :: Num b => b
font_scale = fromIntegral (4 :: Int)

-- Width of the drawing area used by drawReduction.
visual_width :: GLdouble
visual_width = 2000.0

-- Height of the drawing area used by drawReduction
visual_height :: GLdouble
visual_height = 1000.0

-- Distance of the top of the drawing area to the root node of a term.
top_margin :: GLdouble
top_margin = 50.0

-- Maximum number of terms to draw from a reduction.
max_terms :: Int
max_terms = 8

-- Maximum drawing depth of terms.
max_depth :: Int
max_depth = 7

-- Maximum depth for drawing nodes in terms.
--
-- Beyond this depth only edges are drawn.
max_nodes :: Int
max_nodes = 4

-- Maximum redex depth considered during drawing (used for modulus computation).
--
-- Note that this value is completely arbitrary and might not suffice. However,
-- if redexes do occur deeper nothing is likely to be visible anyway, as zoom
-- is limited.
max_reduction_depth :: Integer
max_reduction_depth = 150

-- Data type indicating the drawing area used to draw a particular term.
data SlicePosData
    = SlicePos {
        slice_left   :: GLdouble, -- Left side of the drawing area
        slice_width  :: GLdouble, -- Width of the drawing area
        slice_height :: GLdouble, -- Height of the drawing area
        slice_arrow  :: GLdouble  -- Scale factor for arrow point to next term
      }

-- Data type indicating the drawing area used to draw a particulat subterm.
data PositionData
    = Pos {
        left  :: GLdouble, -- Left side of the drawing area
        right :: GLdouble, -- Right side of the drawing area
        depth :: GLdouble, -- Depth (or height) of the drawing area
        up    :: Maybe (Vector3 GLdouble) -- Position of parent node (if any)
      }

data RelPositionData
    = RelPos {
        rel_left  :: GLdouble,
        rel_inc   :: GLdouble,
        rel_depth :: GLdouble,
        rel_up    :: Maybe (Vector3 GLdouble)
      }

-- Arrow drawing
arrow :: Background -> IO ()
arrow back = do
    -- Save line width and set new line width
    old_width <- get lineWidth
    lineWidth $= 2.0
    color $ case back of -- Arbitrarily chosen colors for arrows
        Black -> Color4 (1.0 :: GLdouble) (153.0 / 255.0) (153.0 / 255.0) 1.0
        White -> Color4 (0.0 :: GLdouble) (153.0 / 255.0) (153.0 / 255.0) 1.0
    -- Draw horizontal line
    renderPrimitive Lines $ do
        vertex $ Vertex3 (-2.0 :: GLdouble) 0.0 0.0
        vertex $ Vertex3 (0.0 :: GLdouble) 0.0 0.0
    -- Draw arrow head
    unsafePreservingMatrix $ do
        rotate (150.0 :: GLdouble) (Vector3 0.0 0.0 (-1.0))
        renderPrimitive Lines $ do
            vertex $ Vertex3 (0.0 :: GLdouble) 0.0 0.0
            vertex $ Vertex3 (1.0 :: GLdouble) 0.0 0.0
    unsafePreservingMatrix $ do
        rotate (150.0 :: GLdouble) (Vector3 0.0 0.0 1.0)
        renderPrimitive Lines $ do
            vertex $ Vertex3 (0.0 :: GLdouble) 0.0 0.0
            vertex $ Vertex3 (1.0 :: GLdouble) 0.0 0.0
    -- Restore old line width
    lineWidth $= old_width

drawArrow :: GLdouble -> Vector3 GLdouble -> Background -> IO ()
drawArrow size location back = do
    unsafePreservingMatrix $ do
        translate location
        scale size size size
        arrow back

drawEdge :: Maybe (Vector3 GLdouble) -> Vector3 GLdouble -> Background -> IO ()
drawEdge Nothing _ _ = do
    return ()
drawEdge (Just up_pos) down_pos back = do
    if back == Black
    then color $ Color4 (153.0 / 255.0 :: GLdouble) (153.0 / 255.0) 1.0 1.0
    else color $ Color4 (153.0 / 255.0 :: GLdouble) (153.0 / 255.0) 0.0 1.0
    renderPrimitive Lines $ do
        vertex $ to_vertex up_pos
        vertex $ to_vertex down_pos
    where to_vertex (Vector3 x y z) = Vertex3 x y (z - 0.5)

node :: TextureObject -> IO ()
node node_texture = do
    textureBinding Texture2D $= Just node_texture
    texture Texture2D $= Enabled
    renderPrimitive TriangleStrip $ do
        texCoord $ TexCoord2 (0.0 :: GLdouble) 0.0
        vertex $ Vertex3 (-1.0 :: GLdouble) (-1.0) 0.0
        texCoord $ TexCoord2 (0.0 :: GLdouble) 1.0
        vertex $ Vertex3 (-1.0 :: GLdouble) 1.0 0.0
        texCoord $ TexCoord2 (1.0 :: GLdouble) 0.0
        vertex $ Vertex3 (1.0 :: GLdouble) (-1.0) 0.0
        texCoord $ TexCoord2 (1.0 :: GLdouble) 1.0
        vertex $ Vertex3 (1.0 :: GLdouble) 1.0 0.0
    texture Texture2D $= Disabled

node_label :: (Show s, Show v, Signature s, Variables v)
    => Symbol s v -> Font -> IO ()
node_label f font = do
    rotate (180.0 :: GLdouble) (Vector3 0.0 0.0 1.0)
    rotate (180.0 :: GLdouble) (Vector3 0.0 1.0 0.0)
    scale size size size
    translate pos
    renderFont font (show f) All
    where size = 0.09 / font_scale :: GLdouble
          pos  = Vector3 x y 0.0
              where x = 15.0 * font_scale :: GLdouble
                    y = -3.0 * font_scale :: GLdouble

getColor :: RewriteSystem s v r
     => Symbol s v -> EnvironmentRef s v r
        -> IO (Color4 GLfloat, Color4 GLfloat)
getColor f environment = do
    env <- get environment
    let (col_b, col_w, cols', gen') = get_color f (colors env) (generator env)
    environment $= env {generator = gen', colors = cols'}
    return (col_b, col_w)
    where get_color sym [] gen
              = (new_col_b, new_col_w, [(sym, new_col_b, new_col_w)], new_gen)
                  where (new_col_b, new_col_w, new_gen) = get_new_color gen
          get_color sym (c@(g, col_b, col_w):cs) gen
              | g == sym  = (col_b,  col_w,  c : cs,  gen)
              | otherwise = (col_b', col_w', c : cs', gen')
                  where (col_b', col_w', cs', gen') = get_color sym cs gen
          get_new_color gen
              = (new_col_b, new_col_w, gen_3)
                  where (r, gen_1) = randomR (0.0, 1.0) gen
                        (g, gen_2) = randomR (0.0, 1.0) gen_1
                        (b, gen_3) = randomR (0.0, 1.0) gen_2
                        r_val_b = (r + 1.0) / 2.0
                        g_val_b = (g + 1.0) / 2.0
                        b_val_b = (b + 1.0) / 2.0
                        r_val_w = (r + 0.5) / 2.0
                        g_val_w = (g + 0.5) / 2.0
                        b_val_w = (b + 0.5) / 2.0
                        new_col_b = Color4 r_val_b g_val_b b_val_b 1.0
                        new_col_w = Color4 r_val_w g_val_w b_val_w 1.0

drawNode :: (Show s, Show v, RewriteSystem s v r)
    => Symbol s v -> Maybe Position -> GLdouble -> Vector3 GLdouble
       -> EnvironmentRef s v r -> IO ()
drawNode f redex_p size location environment = do
    (col_b, col_w) <- getColor f environment
    env <- get environment
    unsafePreservingMatrix $ do
        let red  = Color4 1.0 0.0 0.0 1.0
            col  = if background env == Black then col_b else col_w
            col' = if isJust redex_p && fromJust redex_p == [] then red else col
        color col'
        translate location
        scale size size size
        unsafePreservingMatrix $ do
            node (node_tex env)
        unsafePreservingMatrix $ do
            node_label f (sym_font env)

get_subterms :: (Signature s, Variables v)
    => (Term s v) -> [Term s v]
get_subterms (Function _ ts) = elems ts
get_subterms (Variable _)    = []

drawSubterms :: (Show s, Show v, RewriteSystem s v r)
    => [Term s v] -> Maybe Position -> RelPositionData -> (GLdouble, GLdouble)
       -> (GLdouble, GLdouble) -> Int -> Int -> EnvironmentRef s v r -> IO ()
drawSubterms [] _ _ _ _ _ _ _ = do
    return ()
drawSubterms (s:ss) redex_p rel_pos ul dr max_d max_n environment = do
    drawTerm s redex_p' pos ul dr max_d max_n environment
    drawSubterms ss redex_p'' rel_pos' ul dr max_d max_n environment
    where margin = 0.0
          pos    = Pos {
              left  = rel_left rel_pos + margin,
              right = rel_left rel_pos + rel_inc rel_pos - margin,
              depth = rel_depth rel_pos,
              up    = rel_up rel_pos
              }
          rel_pos' = rel_pos {rel_left = rel_left rel_pos + rel_inc rel_pos}
          (redex_p', redex_p'') = new_position redex_p
          new_position Nothing
              = (Nothing, Nothing)
          new_position (Just (p:ps))
              | p == 1    = (Just ps, Nothing)
              | p > 1     = (Nothing, Just (p - 1:ps))
              | otherwise = error "Illegal position"
          new_position (Just [])
              = (Nothing, Nothing)

drawTerm :: (Show s, Show v, RewriteSystem s v r)
    => Term s v -> Maybe Position -> PositionData -> VisiblePos
       -> VisiblePos -> Int -> Int -> EnvironmentRef s v r -> IO ()
drawTerm term redex_p pos ul dr max_d max_n environment
    | max_d < 0 = do
        return ()
    | fst ul - (2.0 * size) > right pos = do
        env <- get environment
        drawEdge (up pos) location (background env)
        return ()
    | fst dr + (2.0 * size) < left pos = do
        env <- get environment
        drawEdge (up pos) location (background env)
        return ()
    | snd dr + (2.0 * size) < depth pos = do
        env <- get environment
        drawEdge (up pos) location (background env)
        return ()
    | snd ul - (2.0 * size) > depth pos = do
        drawSubterms ss redex_p rel_pos ul dr max_d'' max_n environment
    | max_n <= 0 = do
        env <- get environment
        drawEdge (up pos) location (background env)
        drawSubterms ss redex_p rel_pos ul dr max_d' 0 environment
    | otherwise = do
        env <- get environment
        drawEdge (up pos) location (background env)
        drawSubterms ss redex_p rel_pos ul dr max_d' max_n' environment
        drawNode f redex_p size location environment
        where location = Vector3 ((left pos + right pos) / 2.0) (depth pos) 0.0
              f        = root_symbol term
              ss       = get_subterms term
              max_d'   = max_d - 1
              max_d''  = if up pos == Nothing then max_d' else max_d
              max_n'   = max_n - 1
              count    = length ss
              d_count  = fromIntegral count
              width    = right pos - left pos
              size     = width * 0.02
              rel_pos  = RelPos {
                  rel_left  = if count > 1
                              then left pos
                              else (left pos + width / 4.0),
                  rel_inc   = if count > 1
                              then width / d_count
                              else (width / 2.0),
                  rel_depth = if count > 1
                              then depth pos + width * (d_count - 1.0) / d_count
                              else depth pos + width / 2.0,
                  rel_up    = Just location
                  }

-- Recursively draw the terms of a reduction as far as they exist and would
-- be visible.
drawTerms :: (Show s, Show v, RewriteSystem s v r)
    => [Term s v] -> [Position] -> SlicePosData -> VisiblePos -> VisiblePos
       -> Int -> Int -> Int -> EnvironmentRef s v r -> IO ()
 -- No terms left to draw
drawTerms [] _ _ _ _ _ _ _ _ = do
    return ()
-- A sufficient number of terms have been drawn
drawTerms _ _ _ _ _ 0 _ _ _ = do
    return ()
-- There were an insufficient number of redex positions (should not occur)
drawTerms  (_:_) [] _ _ _ _ _ _ _ = do
    error "Number of terms and positions differ"
-- Normal case where terms should be drawn
drawTerms ts ps slice lu@(_, u_max) rd max_ts max_d max_ns environment
    | slice_height slice + top_margin < u_max = do
        return ()
    | otherwise = do
        env <- get environment
        drawArrow arrow_size arrow_pos (background env)
        drawTerms' ts ps slice lu rd max_ts max_d max_ns environment
        where arrow_size  = slice_arrow slice
              arrow_pos   = Vector3 slice_right top_margin 0.0
              slice_right = slice_left slice + slice_width slice

drawTerms' :: (Show s, Show v, RewriteSystem s v r)
    => [Term s v] -> [Position] -> SlicePosData -> VisiblePos -> VisiblePos
       -> Int -> Int -> Int -> EnvironmentRef s v r -> IO ()
drawTerms' ts ps slice lu@(l_min, _) rd max_ts max_d max_ns environment
    | slice_right < l_min = do -- Current term falls outside the window
        drawTerms ts' ps' slice' lu rd max_ts max_d max_ns environment
    | otherwise = do
        drawTerm t (Just p) t_pos lu rd max_d max_ns environment
        drawTerms ts' ps' slice' lu rd max_ts' max_d' max_ns' environment
        where -- Existence of head and tail is guarenteed by drawTerms
              t:ts' = ts
              p:ps' = ps
              -- Additional slice data
              slice_right = slice_left slice + slice_width slice
              margin      = slice_width slice * 0.025 -- Margin not to draw in
              -- New values for remaining terms
              max_ts' = max_ts - 1
              max_d'  = max 2 (max_d - 1)
              max_ns' = max_ns - 1
              slice' = SlicePos { -- For the next term we have half the space
                  slice_left   = slice_right,
                  slice_width  = slice_width slice / 2.0,
                  slice_height = slice_height slice / 2.0,
                  slice_arrow  = slice_arrow slice / 2.0
                  }
              t_pos = Pos {
                  left  = slice_left slice + margin,
                  right = slice_right - margin,
                  depth = top_margin,
                  up    = Nothing
                  }

-- Helper functions to extract the needed data from reductions.
get_modulus :: RewriteSystem s v r
    => CReduction s v r -> (Integer -> Integer)
get_modulus (CRCons _ phi) = phi'
    where phi' d = ord_to_int (phi ord_zero d)

get_positions :: RewriteSystem s v r
    => CReduction s v r -> [Position]
get_positions (CRCons (RCons _ ss) _) = map fst (get_from ss ord_zero)

get_terms_and_positions :: RewriteSystem s v r
    => CReduction s v r -> ([Term s v], [Position])
get_terms_and_positions reduction = (ts, ps)
    where phi     = get_modulus reduction
          modulus = phi max_reduction_depth
          ts      = genericTake (phi modulus) (get_terms reduction)
          ps      = get_positions reduction

-- drawReduction
drawReduction :: (Show s, Show v, RewriteSystem s v r)
    => EnvironmentRef s v r -> IO ()
drawReduction environment = do
    env <- get environment
    let lu = vis_lu env
        rd = vis_rd env
        (ts, ps) = get_terms_and_positions (env_red env)
        slice    = SlicePos {
            slice_left   = 0.0,
            slice_width  = (visual_width / 2.0), -- Use half for first term
            slice_height = (visual_height - top_margin),
            slice_arrow  = 40.0 -- Arbitrary size for first arrow
            }
    drawTerms ts ps slice lu rd max_terms max_depth max_nodes environment
