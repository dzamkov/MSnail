-----------------------------------------------------------------------------
--
-- Module      :  MSnail.Game
-- Copyright   :  (c) 2010, Dmitry Zamkov
-- License     :  BSD3 (See LICENSE)
--
-----------------------------------------------------------------------------

module MSnail.Game (
	GameInputEvent(..),
	GameInput(..),
	GameOutput(..),
	runGame
) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef
import Data.Time.Clock
import MSnail.FRP
import Control.Arrow

-- A one-time occurence of an input event.
data GameInputEvent	=
	GameStart											|
	KeyEvent Key KeyState Modifiers Position	deriving(Show)

-- State of user and system input at any one time.
data GameInput		=	GameInput {
		updateTime		::	Double,	--	deriviative of current time
		windowWidth		::	Int,
		windowHeight	::	Int,
		inputEvents		::	Event (GameInputEvent)
	}

-- State of game output at any one time.
data GameOutput	=	GameOutput {
		renderFunc		::	IO ()
	}
	
--	Controls the game...
type GameController	=	SF GameInput GameOutput

-- ? (The first argument specifies the amount of seconds in a tick)
runGame	:: GameController -> IO ()
runGame gc =	do
	(progname, _)	<-	getArgsAndInitialize
	initialDisplayMode	$=	[DoubleBuffered]
	createWindow "MSnail"
	windowSize			$=	Size 640 480

	curTime		<-	get elapsedTime
	lastUpdate	<-	newIORef curTime
	curRender	<-	newIORef (return ())
	gameEvents	<-	newIORef ([GameStart])
	
	curGC			<-	newIORef gc
	
	displayCallback	$=	(do
		rf		<-	get curRender
		rf
		flush
		swapBuffers)
	idleCallback		$=	Just (do
		curTime			<-	get elapsedTime
		lastTime			<-	get lastUpdate
		let	delta		= (fromIntegral (curTime - lastTime) / (10 ^ 4))
		
		events			<-	get gameEvents
		
		cgc				<-	get curGC
		let	(ngc, v)	=	injectSF (GameInput delta 640 480 events) cgc
		curGC				$=	ngc
		curRender		$=	renderFunc v
		gameEvents		$=	[]
		
		postRedisplay Nothing)
	keyboardMouseCallback	$=	Just (\key ks mod pos -> do
			events		<-	get gameEvents
			gameEvents	$=	events ++ [KeyEvent key ks mod pos])
	mainLoop