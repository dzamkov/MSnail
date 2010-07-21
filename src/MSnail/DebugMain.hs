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
import MSnail.FRP
import Data.IORef

myshape	=	OctoBlock	[
		SolidBlock (SolidColor 255 0 0),
		OctoBlock	[
			SolidBlock Air,
			SolidBlock Air,
			SolidBlock Air,
			SolidBlock Air,
			SolidBlock Air,
			SolidBlock Air,
			SolidBlock (SolidColor 0 255 0),
			SolidBlock Air
		],
		SolidBlock Air,
		SolidBlock (SolidColor 0 0 255),
		SolidBlock Air,
		SolidBlock Air,
		OctoBlock	[
			SolidBlock Air,
			SolidBlock Air,
			SolidBlock Air,
			SolidBlock Air,
			OctoBlock	[
				SolidBlock Air,
				SolidBlock Air,
				SolidBlock Air,
				SolidBlock Air,
				SolidBlock Air,
				SolidBlock Air,
				SolidBlock (SolidColor 0 255 0),
				SolidBlock Air
			],
			SolidBlock Air,
			SolidBlock (SolidColor 0 255 0),
			SolidBlock Air
		],
		SolidBlock Air
	]

drawBlock	::	Block -> IO ()
drawBlock b	=	res
	where
		ac					=	(Vector (0 :: Double) 0 0, 5.0)
		af (cp, cs) pv	=	(((pNumVector pv) `multVector` (cs * 0.5)) `addVector` cp, cs * 0.5)
		bc					=	[]	::	[([DVector], Double, Double, Double)]
		
		bf	bc (cp, cs) bt	=	case bt of
					(BlockSurface ax pol (SolidColor r g b) Air)	->	(li ax pol False, r, g, b):bc
					(BlockSurface ax pol Air (SolidColor r g b))	->	(li ax pol True, r, g, b):bc
			where
				li axi pol rv	=	map (\l -> cv l) (case rv of
						(True)	->	reverse defli
						(False)	->	defli)
					where
						defli	=	[axisUnorder  x axi | x <- [
										(Vector pol Negative Negative),
										(Vector pol Negative Positive),
										(Vector pol Positive Positive),
										(Vector pol Positive Negative)
									]]
				cv	::	PVector -> DVector
				cv pv	=	addVector (multVector (pNumVector pv) cs) cp
				
		mf Air (SolidColor _ _ _)	=	True
		mf (SolidColor _ _ _) Air	=	True
		mf _ _							=	False
		br			=	surfaceDecompose b af ac bf bc mf
		res		=	renderPrimitive Quads $ foldl (\ac it -> case it of
							(li, r, g, b)	->	do
														ac
														color $ Color3 r g b
														foldl (\ac it -> do
																					ac
																					vertex $ toVertex3 it) (return ()) li
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
					lookAt (Vertex3 10.0 10.0 (10.0 * sin (curang / 100.0))) (Vertex3 0.0 0.0 0.0) (Vector3 0.0 0.0 1.0)
					rotate curang $ Vector3 0 0 (1 :: GLdouble)
					drawBlock myshape
					flush
					swapBuffers)
				idleCallback	$=	(Just $ do
					curang	<-	readIORef angle
					writeIORef angle (curang + 0.1)
					postRedisplay Nothing)
				mainLoop