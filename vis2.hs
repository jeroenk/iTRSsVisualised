import System.Random
import Data.IORef
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import SignatureAndVariables
import Terms
import PositionsAndSubterms
import qualified ExampleTerms

import Array

type SymColor s v = (Symbol s v, Color4 GLdouble)

main :: IO ()
main = do
    gen <- newStdGen
    gen_ref <- newIORef $ gen
    col_ref <- newIORef []
    (program_name, _) <- getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered, RGBAMode, WithDepthBuffer]
    initialWindowSize $= Size 1000 500
    _ <- createWindow program_name
    displayCallback $= display ExampleTerms.f_k_omega gen_ref col_ref
    reshapeCallback $= Just reshape
    clearColor $= Color4 0.0 0.0 0.0 1.0
    depthFunc $= Just Less
    matrixMode $= Projection
    loadIdentity
    ortho 0.0 1000.0 500.0 0.0 (-1.0) (1.0)
    matrixMode $= Modelview 0
    mainLoop

node :: IO ()
node = do
    renderPrimitive Quads $ do
        vertex $ Vertex3 (-1.0::GLdouble) 1.0 0.0
        vertex $ Vertex3 (1.0::GLdouble) 1.0 0.0
        vertex $ Vertex3 (1.0::GLdouble) (-1.0) 0.0
        vertex $ Vertex3 (-1.0::GLdouble) (-1.0) 0.0

getColor :: (Signature s, Variables v)
     => Symbol s v -> (IORef StdGen) -> (IORef [SymColor s v])
        -> IO (Color4 GLdouble)
getColor symbol gen_ref col_ref = do
    generator <- readIORef gen_ref
    colors <- readIORef col_ref
    let (col, colors', gen') = get_color symbol colors generator
    gen_ref $= gen'
    col_ref $= colors'
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
                  where  (r, gen_1) = randomR (0.0, 1.0) gen
                         (g, gen_2) = randomR (0.0, 1.0) gen_1
                         (b, gen_3) = randomR (0.0, 1.0) gen_2
                         r_val = (r + 1.0) / 2.0
                         g_val = (g + 1.0) / 2.0
                         b_val = (b + 1.0) / 2.0

drawNode :: (Signature s, Variables v)
    => Symbol s v -> GLdouble -> (Vector3 GLdouble) -> (IORef StdGen)
       -> (IORef [SymColor s v]) -> IO ()
drawNode symbol size location gen_ref col_ref = do
    unsafePreservingMatrix $ do
        col <- getColor symbol gen_ref col_ref
        color col
        translate location
        scale size size size
        node

get_subterms :: (Signature s, Variables v)
    => (Term s v) -> [Term s v]
get_subterms (Function _ ts) = elems ts
get_subterms (Variable _)    = []

drawSubterms :: (Signature s, Variables v)
    => [Term s v] -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> Int
       -> (IORef StdGen) -> (IORef [SymColor s v]) -> IO ()
drawSubterms [] _ _ _ _ _ _ _ = do
    return ()
drawSubterms (t:ts) left inc down depth depth_left gen_ref col_ref = do
    drawTerm t left (left + inc) down depth depth_left gen_ref col_ref
    drawSubterms ts (left + inc) inc down depth depth_left gen_ref col_ref

drawTerm :: (Signature s, Variables v)
    => (Term s v) -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> Int
       -> (IORef StdGen) -> (IORef [SymColor s v]) -> IO ()
drawTerm term left right down depth depth_left gen_ref col_ref
    | depth_left == 0 = do
        return ()
    | otherwise  = do
        drawNode (get_symbol term []) size location gen_ref col_ref
        drawSubterms subterms left' inc down' depth' depth_left' gen_ref col_ref
        where left' = left + ((right - left) / 50.0)
              right' = right - ((right - left) / 50.0)
              location = Vector3 ((left' + right') / 2.0) depth 0.0
              size = 10.0 / (11.0 - fromIntegral depth_left)
              subterms = get_subterms term
              inc = (right' - left') / (fromIntegral (length subterms))
              depth_left' = depth_left - 1
              down' = down / 1.5
              depth' = depth + down

reshape :: ReshapeCallback
reshape (Size w h) = do
    windowSize $= Size w' h'
    viewport $= (Position 0 0, Size w' h')
    postRedisplay Nothing
        where w' = if (h * 2) > w then w else (h * 2)
              h' = if (h * 2) > w then (w `div` 2) else h

display :: (Signature s, Variables v)
    => (Term s v) -> (IORef StdGen) -> (IORef [SymColor s v]) -> DisplayCallback
display term gen_ref col_ref = do
    clear [ColorBuffer, DepthBuffer]
    drawTerm term 0.0 500.0 160.0 20.0 10 gen_ref col_ref
    drawTerm term 500.0 750.0 (160.0 / 1.5) 20.0 9 gen_ref col_ref
    drawTerm term 750.0 875.0 ((160.0 / 1.5) / 1.5) 20.0 8 gen_ref col_ref
    flush
    swapBuffers
