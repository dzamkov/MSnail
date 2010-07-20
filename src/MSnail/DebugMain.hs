-----------------------------------------------------------------------------
--
-- Module      :  DebugMain
-- Copyright   :  (c) 2010, Dmitry Zamkov
-- License     :  BSD3 (See LICENSE)
--
-----------------------------------------------------------------------------

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import MSnail.Block
import MSnail.Vector
import Data.IORef

myshape	=	createBalloid (SolidColor 0 255 255)

drawBlock	::	Block -> IO ()
drawBlock b	=	res
	where
		ac					=	(Vector (0 :: Double) 0 0, 5.0)
		af (cp, cs) pv	=	(((pNumVector pv) `multVector` (cs * 0.5)) `addVector` cp, cs * 0.5)
		bc					=	[]	::	[(DVector, DVector, DVector, Double, Double, Double)]
		
		bf	bc (cp, cs) bt _	=	case bt of
					(BlockTriangle x y z (SolidColor r g b) Air)	->	(cv x, cv y, cv z, r, g, b):bc
					(BlockTriangle x y z Air (SolidColor r g b))	->	(cv x, cv z, cv y, r, g, b):bc
			where
				cv	::	PVector -> DVector
				cv pv	=	addVector (multVector (pNumVector pv) cs) cp
				
		mf _ _	=	True
		br			=	surfaceDecompose b af ac bf bc mf
		res		=	renderPrimitive Triangles $ foldl (\ac it -> case it of
							(x, y, z, r, g, b)	->	do
																ac
																color $ Color3 r g b
																vertex $ toVertex3 x
																vertex $ toVertex3 y
																vertex $ toVertex3 z
						) (return ()) br

main	=	do
				(progname, _)	<-	getArgsAndInitialize
				initialDisplayMode $= [DoubleBuffered]
				createWindow "MSnail"
				windowSize	$=	Size 640 480
				angle	<-	newIORef 0.0
				polygonMode			$=	(Line, Line)
				cullFace				$=	Just Back
				displayCallback	$=	(do
					curang	<-	readIORef angle
					clear [ColorBuffer]
					matrixMode	$=	Projection
					loadIdentity
					perspective 70.0 (640.0 / 480.0) 0.1 100.0
					lookAt (Vertex3 10.0 10.0 10.0) (Vertex3 0.0 0.0 0.0) (Vector3 0.0 0.0 1.0)
					rotate curang $ Vector3 0 0 (1 :: GLdouble)
					drawBlock myshape
					flush
					swapBuffers)
				idleCallback	$=	(Just $ do
					curang	<-	readIORef angle
					writeIORef angle (curang + 0.1)
					postRedisplay Nothing)
				mainLoop