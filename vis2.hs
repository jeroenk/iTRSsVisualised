module Main (
    main
) where

import Codec.Image.STB
import Control.Exception as E
import Control.Monad
import Data.Bitmap.OpenGL
import Data.IORef
import Data.Maybe
import Graphics.Rendering.OpenGL
import Graphics.Rendering.FTGL
import Graphics.UI.GLUT hiding (Font)
import System.Plugins hiding (Symbol)
import System.Random

import SignatureAndVariables
import Term
import RuleAndSystem
import SystemOfNotation
import Reduction
import DynamicReduction

import Array
import List

type SymbolColor s v = (Symbol s v, Color4 GLfloat)

data RewriteSystem s v r => Environment s v r
    = Env {
        env_red   :: CReduction s v r,
        generator :: StdGen,
        win_size  :: Size,
        vis_ul    :: (GLdouble, GLdouble),
        vis_dr    :: (GLdouble, GLdouble),
        colors    :: [SymbolColor s v],
        mouse_use :: Bool,
        init_pos  :: Position,
        cur_pos   :: Position,
        node_tex  :: TextureObject,
        sym_font  :: Font,
        red_list  :: Maybe DisplayList
      }

type EnvironmentRef s v r = IORef (Environment s v r)

data SlicePosData
    = SlicePos {
        slice_left  :: GLdouble,
        slice_width :: GLdouble,
        slice_depth :: GLdouble,
        slice_arrow :: GLdouble
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

maximum_depth :: Int
maximum_depth = 7

maximum_terms :: Int
maximum_terms = 8

maximum_reduction_depth :: Integer
maximum_reduction_depth = 150

maximum_nodes :: Int
maximum_nodes = 4

init_win_size :: Size
init_win_size = Size 1000 500

font_scale :: Num b => b
font_scale = fromIntegral (4 :: Int)

loadNodeTexture :: String -> IO TextureObject
loadNodeTexture s = do
    stat <- loadImage s
    case stat of
        Left  err -> error $ "loadNode: " ++ err
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
    ortho 0.0 2000.0 1000.0 0.0 (-1.0) 1.0
    matrixMode $= Modelview 0
    loadIdentity

    -- Initialize blending
    blendFunc       $= (SrcAlpha, OneMinusSrcAlpha)
    blend           $= Enabled
    lineSmooth      $= Enabled
    hint LineSmooth $= Nicest

    -- Initialize texture
    tex <- loadNodeTexture "node.png"

    -- Initialize font
    font <- createTextureFont "fonts/FreeSans.ttf"
    _ <- setFontFaceSize font (24 * font_scale) 72

    -- Initialize environment
    gen <- newStdGen
    env <- newIORef $ Env {
        env_red   = red,
        generator = gen,
        win_size  = init_win_size,
        vis_ul    = (0.0, 0.0),
        vis_dr    = (2000.0, 1000.0),
        colors    = [],
        mouse_use = False,
        init_pos  = Position 0 0,
        cur_pos   = Position 0 0,
        node_tex  = tex,
        sym_font  = font,
        red_list  = Nothing
        }

    -- Initialize callbacks
    displayCallback       $= display env
    reshapeCallback       $= Just (reshape env)
    keyboardMouseCallback $= Just (keyboardMouse env)
    motionCallback        $= Just (motion env)

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

drawArrow :: GLdouble -> Vector3 GLdouble -> IO ()
drawArrow size location = do
    unsafePreservingMatrix $ do
        translate location
        scale size size size
        arrow

drawEdge :: Maybe (Vector3 GLdouble) -> Vector3 GLdouble -> IO ()
drawEdge Nothing _ = do
    return ()
drawEdge (Just up_pos) down_pos = do
    color $ Color4 (153.0 / 255.0 :: GLdouble) (153.0 / 255.0) 1.0 1.0
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

node_label :: (Signature s, Variables v, Show s, Show v)
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
     => Symbol s v -> EnvironmentRef s v r -> IO (Color4 GLfloat)
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

drawNode :: (RewriteSystem s v r, Show s, Show v)
    => Symbol s v -> GLdouble -> Vector3 GLdouble -> EnvironmentRef s v r
       -> IO ()
drawNode f size location environment = do
    col <- getColor f environment
    env <- get environment
    unsafePreservingMatrix $ do
        color col
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

drawSubterms :: (RewriteSystem s v r, Show s, Show v)
    => [Term s v] -> RelPositionData -> (GLdouble, GLdouble)
       -> (GLdouble, GLdouble) -> Int -> Int -> EnvironmentRef s v r -> IO ()
drawSubterms [] _ _ _ _ _ _ = do
    return ()
drawSubterms (s:ss) rel_pos ul dr max_d max_n environment = do
    drawTerm s pos ul dr max_d max_n environment
    drawSubterms ss rel_pos' ul dr max_d max_n environment
    where margin = 0.0
          pos    = Pos {
              left  = rel_left rel_pos + margin,
              right = rel_left rel_pos + rel_inc rel_pos - margin,
              depth = rel_depth rel_pos,
              up    = rel_up rel_pos
              }
          rel_pos' = rel_pos {rel_left = rel_left rel_pos + rel_inc rel_pos}

drawTerm :: (RewriteSystem s v r, Show s, Show v)
    => Term s v -> PositionData -> (GLdouble, GLdouble)
       -> (GLdouble, GLdouble) -> Int -> Int -> EnvironmentRef s v r -> IO ()
drawTerm term pos ul dr max_d max_n environment
    | max_d < 0 = do
        return ()
    | fst ul - (2.0 * size) > right pos = do
        drawEdge (up pos) location
        return ()
    | fst dr + (2.0 * size) < left pos = do
        drawEdge (up pos) location
        return ()
    | snd dr + (2.0 * size) < depth pos = do
        drawEdge (up pos) location
        return ()
    | snd ul - (2.0 * size) > depth pos = do
        drawSubterms ss rel_pos ul dr max_d'' max_n environment
    | max_n <= 0 = do
        drawEdge (up pos) location
        drawSubterms ss rel_pos ul dr max_d' 0 environment
    | otherwise = do
        drawEdge (up pos) location
        drawSubterms ss rel_pos ul dr max_d' max_n' environment
        drawNode f size location environment
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

drawTerms :: (RewriteSystem s v r, Show s, Show v)
    => [Term s v] -> SlicePosData -> (GLdouble, GLdouble) -> Int -> Int -> Int
       -> EnvironmentRef s v r -> IO ()
drawTerms [] _ _ _ _ _ _ = do
    return ()
drawTerms _ _ _ 0 _ _ _ = do
    return ()
drawTerms (s:ss) slice (l_min, u_max) max_ts max_d max_ns environment
    | s_left + s_width < l_min = do
        drawArrow arrow_size arrow_loc
        drawTerms ss slice' (l_min, u_max) max_ts max_d max_ns environment
    | slice_depth slice + 50.0 < u_max = do
        return ()
    | otherwise = do
        env <- get environment
        drawArrow arrow_size arrow_loc
        drawTerm s pos (vis_ul env) (vis_dr env) max_d max_ns environment
        drawTerms ss slice' (l_min, u_max) max_ts' max_d' max_ns'  environment
        where start_depth = 50.0
              s_left     = slice_left slice
              s_right    = s_left + s_width
              s_width    = slice_width slice
              s_depth    = slice_depth slice
              margin     = s_width * 0.025
              max_ts'    = max_ts - 1
              max_d'     = max 2 (max_d - 1)
              max_ns'    = max_ns - 1
              arrow_loc  = Vector3 s_right start_depth 0.0
              arrow_size = slice_arrow slice
              slice' = SlicePos {
                  slice_left  = s_right,
                  slice_width = s_width / 2.0,
                  slice_depth = s_depth / 2.0,
                  slice_arrow = arrow_size / 2.0
                  }
              pos = Pos {
                  left  = s_left + margin,
                  right = s_right - margin,
                  depth = start_depth,
                  up    = Nothing
                  }

calc_pos :: (Position, Position) -> ((GLdouble, GLdouble), (GLdouble, GLdouble))
       -> Size -> Bool -> (GLdouble, GLdouble, GLdouble, GLdouble)
calc_pos (Position x y, Position x' y') ((v, w), (v', w')) (Size p q) zoom
    | x == x' && y == y && zoom = (v, w, v', w')
    | otherwise = (x_new, y_new, x_new', y_new')
        where x_new   = v + fromIntegral x * x_scale  :: GLdouble
              y_new   = w + fromIntegral y * y_scale  :: GLdouble
              x_new'  = v + fromIntegral x' * x_scale :: GLdouble
              y_new'  = w + fromIntegral y' * y_scale :: GLdouble
              x_scale = (v' - v) / fromIntegral p
              y_scale = (w' - w) / fromIntegral q

drawMouseSquare :: Bool -> (Position, Position)
       -> ((GLdouble, GLdouble), (GLdouble, GLdouble)) -> Size -> IO ()
drawMouseSquare True poses vis size = do
    unsafePreservingMatrix $ do
        color $ Color4 (255.0 * 0.45 :: GLdouble) (255.0 * 0.95) 0.0 1.0
        let (x_new, y_new, x_new', y_new') = calc_pos poses vis size False
        renderPrimitive LineLoop $ do
            vertex $ Vertex3 x_new  y_new  0.5
            vertex $ Vertex3 x_new  y_new' 0.5
            vertex $ Vertex3 x_new' y_new' 0.5
            vertex $ Vertex3 x_new' y_new  0.5
drawMouseSquare False _ _ _ = do
    return ()

reshape :: (Signature s, Variables v, RewriteSystem s v r)
    => (EnvironmentRef s v r) -> ReshapeCallback
reshape environment (Size w h) = do
    env <- get environment
    environment $= env {win_size = Size w' h'}
    viewport    $= (Position 0 0, Size w' h')
    windowSize  $= Size w' h'
    postRedisplay Nothing
        where w' = if (h * 2) > w then w else (h * 2)
              h' = if (h * 2) > w then (w `div` 2) else h

update_view :: RewriteSystem s v r
    => EnvironmentRef s v r -> IO ()
update_view environment = do
    env <- get environment
    when (isJust $ red_list env) $ deleteObjectNames [fromJust (red_list env)]
    environment $= env {red_list = Nothing}
    let ul = vis_ul env
        dr = vis_dr env
    matrixMode $= Projection
    loadIdentity
    ortho (fst ul) (fst dr) (snd dr) (snd ul) (-1.0) 1.0
    matrixMode $= Modelview 0
    loadIdentity
    postRedisplay Nothing

keyboardMouse :: RewriteSystem s v r
    => EnvironmentRef s v r -> KeyboardMouseCallback
keyboardMouse environment (MouseButton LeftButton) Down _ pos = do
    env <- get environment
    environment $= env {mouse_use = True, init_pos = pos, cur_pos = pos}
    update_view environment -- Just "postRedisplay" has performance issues
keyboardMouse environment (MouseButton LeftButton) Up _ _ = do
    env <- get environment
    let poses = (init_pos env, cur_pos env)
        vis   = (vis_ul env, vis_dr env)
    let (x_new, y_new, x_new', y_new') = calc_pos poses vis (win_size env) True
        x  = min x_new x_new'
        y  = min y_new y_new'
        x' = max x_new x_new'
        y' = max y_new y_new'
    environment $= env {mouse_use = False, vis_ul = (x, y), vis_dr = (x', y')}
    update_view environment
keyboardMouse environment (MouseButton RightButton) Up _ _ = do
    env <- get environment
    environment $= env {vis_ul = (0.0, 0.0), vis_dr = (2000.0, 1000.0)}
    update_view environment
keyboardMouse environment (SpecialKey KeyRight) Down _ _ = do
    env <- get environment
    let (x,  y)  = vis_ul env
        (x', y') = vis_dr env
        move     = (x' - x) * 0.05
    environment $= env {vis_ul = (x + move, y), vis_dr = (x' + move, y')}
    update_view environment
keyboardMouse environment (SpecialKey KeyLeft) Down _ _ = do
    env <- get environment
    let (x,  y)  = vis_ul env
        (x', y') = vis_dr env
        move     = (x' - x) * 0.05
    environment $= env {vis_ul = (x - move, y), vis_dr = (x' - move, y')}
    update_view environment
keyboardMouse environment (SpecialKey KeyUp) Down _ _ = do
    env <- get environment
    let (x,  y)  = vis_ul env
        (x', y') = vis_dr env
        move     = (y' - y) * 0.05
    environment $= env {vis_ul = (x, y - move), vis_dr = (x', y' - move)}
    update_view environment
keyboardMouse environment (SpecialKey KeyDown) Down _ _ = do
    env <- get environment
    let (x,  y)  = vis_ul env
        (x', y') = vis_dr env
        move     = (y' - y) * 0.05
    environment $= env {vis_ul = (x, y + move), vis_dr = (x', y' + move)}
    update_view environment
keyboardMouse environment (Char '+') Down _ _ = do
    env <- get environment
    let (x,  y)  = vis_ul env
        (x', y') = vis_dr env
        x_diff = (x' - x) * 0.05
        y_diff = (y' - y) * 0.05
    environment $= env {vis_ul = (x  + x_diff, y  + y_diff),
                        vis_dr = (x' - x_diff, y' - y_diff)}
    update_view environment
keyboardMouse environment (Char '-') Down _ _ = do
    env <- get environment
    let (x,  y)  = vis_ul env
        (x', y') = vis_dr env
        x_diff = (x' - x) * 0.05
        y_diff = (y' - y) * 0.05
    environment $= env {vis_ul = (x  - x_diff, y  - y_diff),
                        vis_dr = (x' + x_diff, y' + y_diff)}
    update_view environment
keyboardMouse environment (Char 'r') Down _ _ = do
    env <- get environment
    environment $= env {vis_ul = (0.0, 0.0), vis_dr = (2000.0, 1000.0)}
    update_view environment
keyboardMouse _ _ _ _ _ = do
    return ()

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
                        w_new = if (h' * 2) > w' then w' else (h' * 2)
                        h_new = if (h' * 2) > w' then (w' `div` 2) else h'
                        w' = abs (x_cur - x_int)
                        h' = abs (y_cur - y_int)
              x' w = max 0 (min x w)
              y' h = max 0 (min y h)

displayMouseSquare :: RewriteSystem s v r
    => EnvironmentRef s v r -> IO ()
displayMouseSquare environment = do
    env <- get environment
    let poses = (init_pos env, cur_pos env)
        vis   = (vis_ul env, vis_dr env)
    drawMouseSquare (mouse_use env) poses vis (win_size env)

get_modulus :: RewriteSystem s v r
    => CReduction s v r -> (Integer -> Integer)
get_modulus (CRCons _ phi) = phi'
    where phi' d = ord_to_int (phi ord_zero d)

drawReduction :: (RewriteSystem s v r, Show s, Show v)
    => EnvironmentRef s v r -> IO ()
drawReduction environment = do
    dl <- generate_list
    env <- get environment
    environment $= env {red_list = Just dl}
    where generate_list = do
              env <- get environment
              let phi     = get_modulus $ env_red env
                  modulus = phi maximum_reduction_depth
                  ts      = genericTake (phi modulus) (get_terms $ env_red env)
                  slice   = SlicePos 0.0 1000.0 950.0 40.0
                  ul      = vis_ul env
                  max_ts  = maximum_terms
                  max_d   = maximum_depth
                  max_ns  = maximum_nodes
              list <- defineNewList Compile $ do
                  drawTerms ts slice ul max_ts max_d max_ns environment
              return list

displayReduction :: (RewriteSystem s v r, Show s, Show v)
    => EnvironmentRef s v r -> IO ()
displayReduction environment = do
    env <- get environment
    when (isNothing $ red_list env) $ drawReduction environment
    env' <- get environment -- Updated by drawReduction
    callList $ fromJust (red_list env')

displayError :: RewriteSystem s v r
    => EnvironmentRef s v r -> ErrorCall -> IO ()
displayError environment err = do
    -- In case of an error, we reset the drawing area, as the matrices might
    -- have been left in a weird state. We also disable zooming in this case,
    -- as zooming in to an area where now term structure is drawn possibly
    -- circumvents the error.
    clear [ColorBuffer, DepthBuffer]
    env <- get environment
    environment $= env {mouse_use = False,
                        vis_ul    = (0.0, 0.0),
                        vis_dr    = (2000.0, 1000.0)}
    matrixMode $= Projection
    loadIdentity
    ortho 0.0 2000.0 1000.0 0.0 (-1.0) 1.0
    matrixMode $= Modelview 0
    loadIdentity
    putStrLn $ show err

display :: (RewriteSystem s v r, Show s, Show v)
    => EnvironmentRef s v r -> DisplayCallback
display environment = do
    E.catch (do clear [ColorBuffer, DepthBuffer]
                displayReduction environment
                displayMouseSquare environment)
        (displayError environment)
    flush
    swapBuffers
