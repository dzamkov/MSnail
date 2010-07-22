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

-- A one-time occurence of an input event.
data GameInputEvent	=
	GameStart

-- State of user and system input at any one time.
data GameInput		=	GameInput {
		currentTick		::	Integer,
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
	tickCount	<-	newIORef 0
	lastUpdate	<-	newIORef curTime
	curRender	<-	newIORef (return ())
	
	displayCallback	$=	(do
		rf		<-	get curRender
		rf
		flush
		swapBuffers)
	idleCallback		$=	Just (do
		curTime			<-	get elapsedTime
		lastTime			<-	get lastUpdate
		lastUpdate		$=	curTime
		lastTickCount	<-	get tickCount
		tickDelta		<-	return ((curTime - lastTime) * tickRate `div` (10 ^ 4))
		tickCount		$=	lastTickCount + tickDelta
		
		postRedisplay Nothing)
	mainLoop