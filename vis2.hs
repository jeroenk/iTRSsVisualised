import System.Random
import Data.IORef
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

data ProgramState = State {
        generator :: StdGen
    }

main :: IO ()
main = do
    gen <- newStdGen
    state <- newIORef $ State gen
    (program_name, _) <- getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered, RGBAMode, WithDepthBuffer]
    initialWindowSize $= Size 1000 500
    _ <- createWindow program_name
    displayCallback $= display state
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

drawNode :: GLdouble -> (Vector3 GLdouble) -> (Color4 GLdouble) -> IO ()
drawNode size pos col = do
    unsafePreservingMatrix $ do
        color col
        translate pos
        scale size size size
        node

reshape :: ReshapeCallback
reshape (Size w h) = do
    windowSize $= Size w' h'
    viewport $= (Position 0 0, Size w' h')
    postRedisplay Nothing
        where w' = if (h * 2) > w then w else (h * 2)
              h' = if (h * 2) > w then (w `div` 2) else h

display :: (IORef ProgramState) -> DisplayCallback
display state = do
    clear [ColorBuffer, DepthBuffer]
    st <- readIORef state
    let (r, gen_1) = randomR (0.0::Double, 1.0) (generator st)
        (g, gen_2) = randomR (0.0::Double, 1.0) gen_1
        (b, gen_3) = randomR (0.0::Double, 1.0) gen_2
    state $= State gen_3
    drawNode 10.5 pos (col r g b)
    flush
    swapBuffers
    where r_val r = (r + 1.0) / 2.0
          g_val g = (g + 1.0) / 2.0
          b_val b = (b + 1.0) / 2.0
          col r g b = Color4 (r_val r) (g_val g) (b_val b) 1.0
          pos = Vector3 400.0 150.0 0.0
