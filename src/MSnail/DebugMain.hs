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
import Data.IORef

myshape	=	createBalloid (SolidColor 0 255 255)

--drawBlock	::	Block -> IO ()
--drawBlock b	=	res
--	where
--		ac					=	(DVector 0 0 0, 1)
--		af (cp, cs) pv	=	(((pNumVector pv) `multVector` 0.5 `multVector` cs) `addVector` cp, cs * 0.5)
		

main	=	do
				(progname, _)	<-	getArgsAndInitialize
				initialDisplayMode $= [DoubleBuffered]
				createWindow "MSnail"
				windowSize	$=	Size 640 480
				angle	<-	newIORef 0.0
				displayCallback	$=	(do
					curang	<-	readIORef angle
					clear [ColorBuffer]
					matrixMode	$=	Projection
					loadIdentity
					perspective 70.0 (640.0 / 480.0) 0.1 100.0
					lookAt (Vertex3 10.0 10.0 10.0) (Vertex3 0.0 0.0 0.0) (Vector3 0.0 0.0 1.0)
					rotate curang $ Vector3 0 0 (1 :: GLdouble)
					color $ Color3 (0.0 :: GLfloat) (0.5) (1.0)
					renderQuadric (QuadricStyle Nothing NoTextureCoordinates Outside LineStyle) (Sphere 5 5 5)
					flush
					swapBuffers)
				idleCallback	$=	(Just $ do
					curang	<-	readIORef angle
					writeIORef angle (curang + 0.1)
					postRedisplay Nothing)
				mainLoop