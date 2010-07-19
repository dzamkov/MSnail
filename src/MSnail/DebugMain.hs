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

myshape	=	createBalloid (SolidColor 0 255 255)

main	=	do
				createWindow "MSnail"
				displayCallback	$=	do
												clear [ColorBuffer]
												flush
				mainLoop
