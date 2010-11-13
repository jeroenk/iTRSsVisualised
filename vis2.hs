import System.Random
import Data.IORef
import Control.Monad
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import System.Plugins hiding (Symbol)

import SignatureAndVariables
import Terms
import PositionsAndSubterms hiding (pos)
import RulesAndSystems
import OmegaReductions

import Array

type SymbolColor s v = (Symbol s v, Color4 GLfloat)

data (Signature s, Variables v, RewriteSystem s v r) => Environment s v r
    = Env {
        win_size  :: Size,
        env_red   :: CReduction s v r,
        generator :: StdGen,
        colors    :: [SymbolColor s v],
        mouse_use :: Bool,
        init_pos  :: Position,
        cur_pos   :: Position
      }

type EnvironmentRef s v r = IORef (Environment s v r)

data TermPosData
    = TermPos {
        left_pos :: GLdouble,
        width    :: GLdouble,
        count    :: Int
      }

data PositionData
    = Pos {
        left  :: GLdouble,
        right :: GLdouble,
        down  :: GLdouble,
        depth :: GLdouble,
        up    :: Maybe (Vector3 GLdouble)
      }

data RelPositionData
    = RelPos {
        rel_left  :: GLdouble,
        rel_inc   :: GLdouble,
        rel_down  :: GLdouble,
        rel_depth :: GLdouble,
        rel_up    :: Maybe (Vector3 GLdouble)
      }

maximum_depth :: Int
maximum_depth = 14

maximum_terms :: Int
maximum_terms = 7

init_win_size :: Size
init_win_size = Size 1000 500

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
        where to_string []     = ""
              to_string (x:xs) = x ++ "\n" ++ to_string xs

main :: IO ()
main = do
    red <- loadReduction "ExampleReduction"
    gen <- newStdGen
    env <- newIORef $ Env {
        win_size  = init_win_size,
        env_red   = red,
        generator = gen,
        colors    = [],
        mouse_use = False,
        init_pos  = Position 0 0,
        cur_pos   = Position 0 0
      }
    (program_name, _) <- getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered, RGBAMode, WithDepthBuffer]
    initialWindowSize $= init_win_size
    _ <- createWindow program_name
    displayCallback $= display env
    reshapeCallback $= Just (reshape env)
    keyboardMouseCallback $= Just (keyboardMouse env)
    motionCallback $= Just (motion env)
    clearColor $= Color4 0.0 0.0 0.0 1.0
    depthFunc $= Just Less
    matrixMode $= Projection
    loadIdentity
    ortho 0.0 2000.0 1000.0 0.0 (-1.0) 1.0
    matrixMode $= Modelview 0
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    blend $= Enabled
    lineSmooth $= Enabled
    hint LineSmooth $= Nicest
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
    renderPrimitive TriangleStrip $ do
        vertex $ Vertex3 (-1.0 :: GLdouble) (-1.0) 0.0
        vertex $ Vertex3 (-1.0 :: GLdouble) 1.0 0.0
        vertex $ Vertex3 (1.0 :: GLdouble) (-1.0) 0.0
        vertex $ Vertex3 (1.0 :: GLdouble) 1.0 0.0

getColor :: (Signature s, Variables v, RewriteSystem s v r)
     => Symbol s v -> (EnvironmentRef s v r) -> IO (Color4 GLfloat)
getColor symbol environment = do
    env <- get environment
    let (col, cols', gen') = get_color symbol (colors env) (generator env)
    environment $= env {generator = gen', colors = cols'}
    return col
    where get_color sym [] gen
              = (new_color, [(sym, new_color)], new_gen)
                  where (new_color, new_gen) = get_new_color gen
          get_color sym (c:cs) gen
              | fst c == sym = (snd c, c:cs, gen)
              | otherwise    = (c', c:cs', gen')
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
drawNode symbol size location environment = do
    unsafePreservingMatrix $ do
        col <- getColor symbol environment
        translate location
        scale size size size
        node col

get_subterms :: (Signature s, Variables v)
    => (Term s v) -> [Term s v]
get_subterms (Function _ ts) = elems ts
get_subterms (Variable _)    = []

drawSubterms :: (Signature s, Variables v, RewriteSystem s v r)
    => [Term s v] -> RelPositionData -> Int -> (EnvironmentRef s v r) -> IO ()
drawSubterms [] _ _ _ = do
    return ()
drawSubterms (t:ts) rel_pos depth_left environment = do
    drawTerm t pos_new depth_left environment
    drawSubterms ts rel_pos_new depth_left environment
    where -- drawTerm call
          pos_new = Pos {
              left = rel_left rel_pos,
              right = rel_left rel_pos + rel_inc rel_pos,
              down = rel_down rel_pos,
              depth = rel_depth rel_pos,
              up = rel_up rel_pos
            }
          -- drawSubterms call
          rel_pos_new = rel_pos {rel_left = rel_left rel_pos + rel_inc rel_pos}

drawTerm :: (Signature s, Variables v, RewriteSystem s v r)
    => (Term s v) -> PositionData -> Int -> (EnvironmentRef s v r) -> IO ()
drawTerm term pos depth_left environment
    | depth_left == 0 = do
        return ()
    | otherwise  = do
        drawEdge (up pos) location
        drawSubterms subterms rel_pos depth_left' environment
        drawNode sym size location environment
        where -- Shared data
              left' = left pos + ((right pos - left pos) / 50.0)
              right' = right pos - ((right pos - left pos) / 50.0)
              -- drawNode call
              sym = get_symbol term []
              size = 20.0 / (fromIntegral (1 + maximum_depth - depth_left))
              location = Vector3 ((left' + right') / 2.0) (depth pos) 0.0
              -- drawSubterms call
              subterms = get_subterms term
              rel_pos = RelPos {
                  rel_left = left',
                  rel_inc = (right' - left') / (fromIntegral (length subterms)),
                  rel_down = down pos / 1.5,
                  rel_depth = depth pos + down pos,
                  rel_up = Just location
              }
              depth_left' = depth_left - 1

drawTerms :: (Signature s, Variables v, RewriteSystem s v r)
    => [Term s v] -> TermPosData -> (EnvironmentRef s v r) -> IO ()
drawTerms [] _ _ = do
    return ()
drawTerms (s:ss) pos environment
    | count pos == maximum_terms = do
        return ()
    | otherwise = do
        drawTerm s term_pos (maximum_depth - count pos) environment
        drawArrow arrow_size (Vector3 arrow_pos 40.0 0.0)
        drawTerms ss pos_new environment
            where right_pos = left_pos pos + width pos
                  arrow_size = 40.0 / 2.0^(count pos)
                  arrow_pos = right_pos - ((2000.0 - right_pos) / 50.0)
                  term_pos = Pos {
                      left = left_pos pos,
                      right = right_pos,
                      down = 320.0 / (1.5^(count pos)),
                      depth = 40.0,
                      up = Nothing
                    }
                  pos_new = TermPos {
                      left_pos = right_pos,
                      width = (width pos) / 2,
                      count = count pos + 1
                    }

drawMouseSquare :: Bool -> Position -> Position -> Size -> IO ()
drawMouseSquare True (Position x y) (Position x' y') (Size w h) = do
    unsafePreservingMatrix $ do
        color $ Color4 (255.0 * 0.45 :: GLdouble) (255.0 * 0.95) 0.0 1.0
        renderPrimitive LineLoop $ do
            vertex $ Vertex3 x_new  y_new  0.0
            vertex $ Vertex3 x_new  y_new' 0.0
            vertex $ Vertex3 x_new' y_new' 0.0
            vertex $ Vertex3 x_new' y_new  0.0
        where x_new   = fromIntegral x * x_scale :: GLdouble
              y_new   = fromIntegral y * y_scale :: GLdouble
              x_new'  = fromIntegral x' * x_scale :: GLdouble
              y_new'  = fromIntegral y' * y_scale :: GLdouble
              x_scale = 2000.0 / fromIntegral w
              y_scale = 1000.0 / fromIntegral h
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

keyboardMouse :: (Signature s, Variables v, RewriteSystem s v r)
    => (EnvironmentRef s v r) -> KeyboardMouseCallback
keyboardMouse environment (MouseButton LeftButton) Down _ pos = do
    env <- get environment
    environment $= env {mouse_use = True, init_pos = pos, cur_pos = pos}
    postRedisplay Nothing
keyboardMouse environment (MouseButton LeftButton) Up _ _ = do
    env <- get environment
    environment $= env {mouse_use = False}
    -- matrixMode $= Projection
    -- loadIdentity
    -- ortho 1750.0 2000.0 125.0 0.0 (-1.0) (1.0)
    -- matrixMode $= Modelview 0
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
                            | otherwise   = x_int - w_new
                        y_new
                            | y_cur >= y_int = y_int + h_new
                            | otherwise   = y_int - h_new
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
    let terms = get_terms (env_red env)
    drawTerms terms (TermPos 0.0 1000.0 0) environment
    drawMouseSquare (mouse_use env) (init_pos env) (cur_pos env) (win_size env)
    flush
    swapBuffers
