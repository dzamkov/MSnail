-----------------------------------------------------------------------------
--
-- Module      :  DebugMain
-- Copyright   :  (c) 2010, Dmitry Zamkov
-- License     :  BSD3 (See LICENSE)
--
-----------------------------------------------------------------------------

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

main	=	do
				createWindow "MSnail"
				displayCallback	$=	clear [ColorBuffer]
				mainLoop
