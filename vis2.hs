module Main (
    main
) where

import Codec.Image.STB
import Control.Monad
import Data.Bitmap.OpenGL
import Data.IORef
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import System.Plugins hiding (Symbol)
import System.Random
import System.Exit

import SignatureAndVariables
import Term
import PositionAndSubterm hiding (Position, pos)
import RuleAndSystem
import OmegaReduction
import DynamicOmegaReduction

import Array

type SymbolColor s v = (Symbol s v, Color4 GLfloat)

data (Signature s, Variables v, RewriteSystem s v r) => Environment s v r
    = Env {
        env_red   :: CReduction s v r,
        cur_depth :: Int,
        generator :: StdGen,
        win_size  :: Size,
        vis_ul    :: (GLdouble, GLdouble),
        vis_dr    :: (GLdouble, GLdouble),
        zoom_fac  :: GLdouble,
        colors    :: [SymbolColor s v],
        mouse_use :: Bool,
        init_pos  :: Position,
        cur_pos   :: Position
      }

type EnvironmentRef s v r = IORef (Environment s v r)

data SlicePosData
    = SlicePos {
        slice_left  :: GLdouble,
        slice_width :: GLdouble,
        slice_depth :: GLdouble
      }

data PositionData
    = Pos {
        left  :: GLdouble,
        right :: GLdouble,
        depth :: GLdouble,
        up    :: Maybe (Vector3 GLdouble)
      }

data RelPositionData
    = RelPos {
        rel_left  :: GLdouble,
        rel_inc   :: GLdouble,
        rel_depth :: GLdouble,
        rel_up    :: Maybe (Vector3 GLdouble)
      }

-- Limit on the maximum depth of the trees being displayed.
--
-- Notice that this depth is beyond what can be reasonably drawn without running
-- into rounding errors
maximum_depth :: Int
maximum_depth = 100

maximum_terms :: Int
maximum_terms = 10

init_win_size :: Size
init_win_size = Size 1000 500

loadNodeTexture :: IO TextureObject
loadNodeTexture = do
    stat <- loadImage "node.png"
    case stat of
        Left err  -> error $ "loadNode: " ++ err
        Right img -> makeSimpleBitmapTexture img

loadReduction :: String -> IO (CReduction DynamicSigma DynamicVar DynamicSystem)
loadReduction s = do
    make_stat <- make (s ++ ".hs") ["-i.."]
    case make_stat of
        MakeFailure err -> error $ to_string err
        MakeSuccess _ _ -> return ()
    load_stat <- load (s ++ ".o") [".."] [] "c_reduction"
    case load_stat of
        LoadFailure err -> error $ to_string err
        LoadSuccess _ v -> return v
        where to_string = foldr (\x y -> x ++ "\n" ++ y) ""

main :: IO ()
main = do
    -- Initialize environment
    (program_name, args) <- getArgsAndInitialize

    reduction_file <- case args of
        [fn] -> return (if   drop (length fn - 3) fn == ".hs"
                        then take (length fn - 3) fn
                        else fn)
        _    -> error $ "usage: " ++ program_name ++ " <reduction>"

    red <- loadReduction reduction_file
    gen <- newStdGen
    env <- newIORef $ Env {
        env_red   = red,
        cur_depth = 0,
        generator = gen,
        win_size  = init_win_size,
        vis_ul    = (0.0, 0.0),
        vis_dr    = (2000.0, 1000.0),
        zoom_fac  = 1.0,
        colors    = [],
        mouse_use = False,
        init_pos  = Position 0 0,
        cur_pos   = Position 0 0
        }

    -- Initialize window
    initialDisplayMode $= [DoubleBuffered, RGBAMode, WithDepthBuffer]
    initialWindowSize  $= init_win_size
    _ <- createWindow program_name

    -- Initialize callbacks
    displayCallback       $= display env
    reshapeCallback       $= Just (reshape env)
    keyboardMouseCallback $= Just (keyboardMouse env)
    motionCallback        $= Just (motion env)

    -- Initialize projections and blending
    clearColor $= Color4 0.0 0.0 0.0 1.0
    depthFunc  $= Just Less
    matrixMode $= Projection
    loadIdentity
    ortho 0.0 2000.0 1000.0 0.0 (-1.0) 1.0
    matrixMode $= Modelview 0
    blendFunc  $= (SrcAlpha, OneMinusSrcAlpha)
    blend      $= Enabled
    lineSmooth $= Enabled
    hint LineSmooth $= Nicest

    -- Initialize texture
    tex <- loadNodeTexture
    textureBinding Texture2D $= Just tex

    -- Main loop
    mainLoop

arrow :: IO ()
arrow = do
    old_width <- get lineWidth
    lineWidth $= 2.0
    color $ Color4 (1.0 :: GLdouble) (153.0 / 255.0) (153.0 / 255.0) 1.0
    renderPrimitive Lines $ do
        vertex $ Vertex3 (-2.0 :: GLdouble) 0.0 0.0
        vertex $ Vertex3 (0.0 :: GLdouble) 0.0 0.0
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
    lineWidth $= old_width

drawArrow :: GLdouble -> (Vector3 GLdouble) -> IO ()
drawArrow size location = do
    unsafePreservingMatrix $ do
        translate location
        scale size size size
        arrow

drawEdge :: (Maybe (Vector3 GLdouble)) -> (Vector3 GLdouble) -> IO ()
drawEdge Nothing _ = do
    return ()
drawEdge (Just up_pos) down_pos = do
    color $ Color4 (153.0 / 255.0 :: GLdouble) (153.0 / 255.0) 1.0 1.0
    renderPrimitive Lines $ do
        vertex $ to_vertex up_pos
        vertex $ to_vertex down_pos
    where to_vertex (Vector3 x y z) = Vertex3 x y (z - 0.5)

node :: (Color4 GLfloat) -> IO ()
node col = do
    color col
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

getColor :: (Signature s, Variables v, RewriteSystem s v r)
     => Symbol s v -> (EnvironmentRef s v r) -> IO (Color4 GLfloat)
getColor f environment = do
    env <- get environment
    let (col, cols', gen') = get_color f (colors env) (generator env)
    environment $= env {generator = gen', colors = cols'}
    return col
    where get_color sym [] gen
              = (new_color, [(sym, new_color)], new_gen)
                  where (new_color, new_gen) = get_new_color gen
          get_color sym (c:cs) gen
              | fst c == sym = (snd c, c : cs, gen)
              | otherwise    = (c', c : cs', gen')
                  where (c', cs', gen') = get_color sym cs gen
          get_new_color gen
              = (Color4 r_val g_val b_val 1.0, gen_3)
                  where (r, gen_1) = randomR (0.0, 1.0) gen
                        (g, gen_2) = randomR (0.0, 1.0) gen_1
                        (b, gen_3) = randomR (0.0, 1.0) gen_2
                        r_val = (r + 1.0) / 2.0
                        g_val = (g + 1.0) / 2.0
                        b_val = (b + 1.0) / 2.0

drawNode :: (Signature s, Variables v, RewriteSystem s v r)
    => Symbol s v -> GLdouble -> (Vector3 GLdouble) -> (EnvironmentRef s v r)
       -> IO ()
drawNode f size location environment = do
    unsafePreservingMatrix $ do
        col <- getColor f environment
        translate location
        scale size size size
        node col

get_subterms :: (Signature s, Variables v)
    => (Term s v) -> [Term s v]
get_subterms (Function _ ts) = elems ts
get_subterms (Variable _)    = []

drawSubterms :: (Signature s, Variables v, RewriteSystem s v r)
    => [Term s v] -> RelPositionData -> GLdouble -> GLdouble -> GLdouble
       -> GLdouble -> GLdouble -> (EnvironmentRef s v r) -> IO ()
drawSubterms [] _ _ _ _ _ _ _ = do
    return ()
drawSubterms (s:ss) rel_pos l_min r_max d_max depth_lim zoom environment = do
    drawTerm s pos l_min r_max d_max depth_lim zoom environment
    drawSubterms ss rel_pos' l_min r_max d_max depth_lim zoom environment
    where margin = 0.0
          pos    = Pos {
              left  = rel_left rel_pos + margin,
              right = rel_left rel_pos + rel_inc rel_pos - margin,
              depth = rel_depth rel_pos,
              up    = rel_up rel_pos
              }
          rel_pos' = rel_pos {rel_left = rel_left rel_pos + rel_inc rel_pos}

drawTerm :: (Signature s, Variables v, RewriteSystem s v r)
    => (Term s v) -> PositionData -> GLdouble -> GLdouble -> GLdouble
       -> GLdouble -> GLdouble -> (EnvironmentRef s v r) -> IO ()
drawTerm term pos l_min r_max d_max depth_lim zoom environment
    | (depth pos - 50.0) > depth_lim = do
        return ()
    | l_min > right pos = do
        drawEdge (up pos) location
        return ()
    | r_max < left pos = do
        drawEdge (up pos) location
        return ()
    | d_max < depth pos = do
        drawEdge (up pos) location
        return ()
    | otherwise = do
        drawEdge (up pos) location
        drawSubterms ss rel_pos l_min r_max d_max depth_lim zoom environment
        drawNode f size location environment
        where location = Vector3 ((left pos + right pos) / 2.0) (depth pos) 0.0
              f        = get_symbol term []
              ss       = get_subterms term
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

drawTerms :: (Signature s, Variables v, RewriteSystem s v r)
    => [Term s v] -> SlicePosData -> GLdouble -> GLdouble -> GLdouble
       -> GLdouble -> Int -> (EnvironmentRef s v r) -> IO ()
drawTerms [] _ _ _ _ _ _ _ = do
    return ()
drawTerms _ _ _ _ _ _ 0 _ = do
    return ()
drawTerms (s:ss) slice zoom l_min r_max d_max max_terms environment
    | left_side + width < l_min = do
        drawTerms ss slice' zoom l_min r_max d_max max_terms environment
    | otherwise = do
        drawTerm s pos l_min r_max d_max depth_lim zoom environment
        drawTerms ss slice' zoom l_min r_max d_max max_terms' environment
        where left_side  = slice_left slice
              width      = slice_width slice
              max_depth  = slice_depth slice
              depth_pct  = 1.0 - 1.0 / (1000.0 * zoom)
              depth_lim  = max_depth * depth_pct
              max_terms' = max_terms - 1
              slice' = SlicePos {
                  slice_left  = left_side + width,
                  slice_width = width / 2.0,
                  slice_depth = max_depth / 2.0
                  }
              pos = Pos {
                  left  = left_side + width * 0.025,
                  right = left_side + width - width * 0.025,
                  depth = 50.0,
                  up    = Nothing
                  }

drawMouseSquare :: Bool -> (Position, Position)
                   -> ((GLdouble, GLdouble), (GLdouble, GLdouble)) -> Size
                   -> IO ()
drawMouseSquare True (Position x y, Position x' y') ((v, w), (v', w')) (Size p q) = do
    unsafePreservingMatrix $ do
        color $ Color4 (255.0 * 0.45 :: GLdouble) (255.0 * 0.95) 0.0 1.0
        renderPrimitive LineLoop $ do
            vertex $ Vertex3 x_new  y_new  0.5
            vertex $ Vertex3 x_new  y_new' 0.5
            vertex $ Vertex3 x_new' y_new' 0.5
            vertex $ Vertex3 x_new' y_new  0.5
        where x_new   = v + fromIntegral x * x_scale :: GLdouble
              y_new   = w + fromIntegral y * y_scale :: GLdouble
              x_new'  = v + fromIntegral x' * x_scale :: GLdouble
              y_new'  = w + fromIntegral y' * y_scale :: GLdouble
              x_scale = (v' - v) / fromIntegral p
              y_scale = (w' - w) / fromIntegral q
drawMouseSquare False _ _ _ = do
    return ()

reshape :: (Signature s, Variables v, RewriteSystem s v r)
    => (EnvironmentRef s v r) -> ReshapeCallback
reshape environment (Size w h) = do
    env <- get environment
    environment $= env {win_size = Size w' h'}
    viewport $= (Position 0 0, Size w' h')
    windowSize $= Size w' h'
    postRedisplay Nothing
        where w' = if (h * 2) > w then w else (h * 2)
              h' = if (h * 2) > w then (w `div` 2) else h

calc_pos :: (Position, Position) -> ((GLdouble, GLdouble), (GLdouble, GLdouble))
            -> Size -> (GLdouble, GLdouble, GLdouble, GLdouble)
calc_pos (Position x y, Position x' y') ((v, w), (v', w')) (Size p q)
    | x == x' && y == y = (v, w, v', w')
    | otherwise = (x_new, y_new, x_new', y_new')
        where x_new   = v + fromIntegral x * x_scale  :: GLdouble
              y_new   = w + fromIntegral y * y_scale  :: GLdouble
              x_new'  = v + fromIntegral x' * x_scale :: GLdouble
              y_new'  = w + fromIntegral y' * y_scale :: GLdouble
              x_scale = (v' - v) / fromIntegral p
              y_scale = (w' - w) / fromIntegral q

keyboardMouse :: (Signature s, Variables v, RewriteSystem s v r)
    => (EnvironmentRef s v r) -> KeyboardMouseCallback
keyboardMouse environment (MouseButton LeftButton) Down _ pos = do
    env <- get environment
    environment $= env {mouse_use = True, init_pos = pos, cur_pos = pos}
    postRedisplay Nothing
keyboardMouse environment (MouseButton LeftButton) Up _ _ = do
    env <- get environment
    let (x_new, y_new, x_new', y_new') = calc_pos (init_pos env, cur_pos env) (vis_ul env, vis_dr env) (win_size env)
    let x = min x_new x_new'
    let y = min y_new y_new'
    let x' = max x_new x_new'
    let y' = max y_new y_new'
    environment $= env {mouse_use = False,
                        vis_ul    = (x, y),
                        vis_dr    = (x', y'),
                        zoom_fac  = 2000.0 / (x' - x)}
    matrixMode $= Projection
    loadIdentity
    ortho x x' y' y (-1.0) 1.0
    matrixMode $= Modelview 0
    postRedisplay Nothing
keyboardMouse environment (MouseButton RightButton) Up _ _ = do
    env <- get environment
    environment $= env {mouse_use = False,
                        vis_ul    = (0.0, 0.0),
                        vis_dr    = (2000.0, 1000.0),
                        zoom_fac  = 1.0}
    matrixMode $= Projection
    loadIdentity
    ortho 0.0 2000.0 1000.0 0.0 (-1.0) 1.0
    matrixMode $= Projection
    postRedisplay Nothing
keyboardMouse _ _ _ _ _ = do
    return ()

motion :: (Signature s, Variables v, RewriteSystem s v r)
    => (EnvironmentRef s v r) -> MotionCallback
motion environment (Position x y) = do
    env <- get environment
    let mouse = mouse_use env
    let (Size w h) = win_size env
    let (Position x_int y_int) = init_pos env
    when mouse $ environment $= env {cur_pos = pos x_int (x' w) y_int (y' h)}
    postRedisplay Nothing
        where pos x_int x_cur y_int y_cur = Position x_new y_new
                  where x_new
                            | x_cur >= x_int = x_int + w_new
                            | otherwise      = x_int - w_new
                        y_new
                            | y_cur >= y_int = y_int + h_new
                            | otherwise      = y_int - h_new
                        w_new = if (h' * 2) > w' then w' else (h' * 2)
                        h_new = if (h' * 2) > w' then (w' `div` 2) else h'
                        w' = abs (x_cur - x_int)
                        h' = abs (y_cur - y_int)
              x' w = max 0 (min x w)
              y' h = max 0 (min y h)

display :: (Signature s, Variables v, RewriteSystem s v r)
    => (EnvironmentRef s v r) -> DisplayCallback
display environment = do
    clear [ColorBuffer, DepthBuffer]
    env <- get environment
    let phi = get_modulus (env_red env)
    let terms = take (phi maximum_depth) (get_terms (env_red env))
    drawTerms terms (SlicePos 0.0 1000.0 950.0) (zoom_fac env) (fst $ vis_ul env) (fst $ vis_dr env) (snd $ vis_dr env) maximum_terms environment
    drawMouseSquare (mouse_use env) (init_pos env, cur_pos env) (vis_ul env, vis_dr env) (win_size env)
    flush
    swapBuffers
