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
import MSnail.Game
import Data.IORef

game	::	SF GameInput GameOutput
game	=	proc (gi)	->	do
				let	tick	=	currentTick gi
				let	com	=	putStrLn (show (tick))
				IdentitySF	-<	GameOutput com
			
main	=	runGame game