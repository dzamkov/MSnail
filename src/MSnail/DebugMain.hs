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
				tick	<-	accumSF 0 (\_ x -> x + 1)					-<	gi
				time	<-	accumSF 0 (\i x -> x + (updateTime i))	-<	gi
				let	com	=	putStrLn (show (tick) ++ " : " ++ show(time))
				returnSF	-<	GameOutput com
			
main	=	runGame game