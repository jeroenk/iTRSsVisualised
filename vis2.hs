import System.Random
import Data.IORef
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import SignatureAndVariables
import Terms
import PositionsAndSubterms
import qualified ExampleTerms

import Array

type SymbolColor s v = (Symbol s v, Color4 GLdouble)

data (Signature s, Variables v) => Environment s v
    = Env {
        generator :: StdGen,
        colors    :: [SymbolColor s v]
      }

type EnvironmentRef s v = IORef (Environment s v)

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

main :: IO ()
main = do
    gen <- newStdGen
    let cols :: [SymbolColor ExampleTerms.Sigma ExampleTerms.Var] = []
    env <- newIORef $ Env {generator = gen, colors = cols}
    (program_name, _) <- getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered, RGBAMode, WithDepthBuffer]
    initialWindowSize $= Size 1000 500
    _ <- createWindow program_name
    displayCallback $= display ExampleTerms.h_omega env
    reshapeCallback $= Just reshape
    clearColor $= Color4 0.0 0.0 0.0 1.0
    depthFunc $= Just Less
    matrixMode $= Projection
    loadIdentity
    ortho 0.0 1000.0 500.0 0.0 (-1.0) (1.0)
    matrixMode $= Modelview 0
    mainLoop

arrow :: IO ()
arrow = do
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

node :: IO ()
node = do
    renderPrimitive Quads $ do
        vertex $ Vertex3 (-1.0 :: GLdouble) 1.0 0.0
        vertex $ Vertex3 (1.0 :: GLdouble) 1.0 0.0
        vertex $ Vertex3 (1.0 :: GLdouble) (-1.0) 0.0
        vertex $ Vertex3 (-1.0 :: GLdouble) (-1.0) 0.0

getColor :: (Signature s, Variables v)
     => Symbol s v -> (EnvironmentRef s v) -> IO (Color4 GLdouble)
getColor symbol environment = do
    env <- readIORef environment
    let (col, cols', gen') = get_color symbol (colors env) (generator env)
    environment $= Env {generator = gen', colors = cols'}
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

drawNode :: (Signature s, Variables v)
    => Symbol s v -> GLdouble -> (Vector3 GLdouble) -> (EnvironmentRef s v)
       -> IO ()
drawNode symbol size location environment = do
    unsafePreservingMatrix $ do
        col <- getColor symbol environment
        color col
        translate location
        scale size size size
        node

get_subterms :: (Signature s, Variables v)
    => (Term s v) -> [Term s v]
get_subterms (Function _ ts) = elems ts
get_subterms (Variable _)    = []

drawSubterms :: (Signature s, Variables v)
    => [Term s v] -> RelPositionData -> Int -> (EnvironmentRef s v) -> IO ()
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

drawTerm :: (Signature s, Variables v)
    => (Term s v) -> PositionData -> Int -> (EnvironmentRef s v) -> IO ()
drawTerm term pos depth_left environment
    | depth_left == 0 = do
        return ()
    | otherwise  = do
        drawEdge (up pos) location
        drawNode sym size location environment
        drawSubterms subterms rel_pos depth_left' environment
        where -- Shared data
              left' = left pos + ((right pos - left pos) / 50.0)
              right' = right pos - ((right pos - left pos) / 50.0)
              -- drawNode call
              sym = get_symbol term []
              size = 10.0 / (fromIntegral (1 + maximum_depth - depth_left))
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

reshape :: ReshapeCallback
reshape (Size w h) = do
    windowSize $= Size w' h'
    viewport $= (Position 0 0, Size w' h')
    postRedisplay Nothing
        where w' = if (h * 2) > w then w else (h * 2)
              h' = if (h * 2) > w then (w `div` 2) else h

display :: (Signature s, Variables v)
    => (Term s v) -> (EnvironmentRef s v) -> DisplayCallback
display term environment = do
    clear [ColorBuffer, DepthBuffer]
    drawTerm term (Pos 0.0 500.0 160.0 20.0 Nothing) maximum_depth environment
    drawArrow (20.0 / 1.0) (Vector3 (500.0 - (500.0 / 50.0)) 20.0 0.0)
    drawTerm term (Pos 500.0 750.0 (160.0 / 1.5) 20.0 Nothing) (maximum_depth - 1) environment
    drawArrow (20.0 / 2.0) (Vector3 (750.0 - (250.0 / 50.0)) 20.0 0.0)
    drawTerm term (Pos 750.0 875.0 ((160.0 / 1.5) / 1.5) 20.0 Nothing) (maximum_depth - 2) environment
    drawArrow (20.0 / 3.0) (Vector3 (875.0 - (125.0 / 50.0)) 20.0 0.0)
    flush
    swapBuffers
