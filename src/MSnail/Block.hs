-----------------------------------------------------------------------------
--
-- Module      :  MSnail.Block
-- Copyright   :  (c) 2010, Dmitry Zamkov
-- License     :  BSD3 (See LICENSE)
--
-----------------------------------------------------------------------------

module MSnail.Block (
	Axis(..),
	Polarity(..),
	polIndex,
	polList,
	axisOrder,
	Block(..),
	Material(..),
	subBlock,
	createBalloid
) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import MSnail.Vector

-- Represents a finite cubic region made from materials. Blocks have no real-world size
-- and all information about the blocks contents must be specified in the block itself. Blocks
-- have both visual and physical properties.

data Block	=
	OctoBlock [Block]														|
	SlopedBlock	Axis Polarity Material Material					|
	CornerBlock Polarity Polarity Polarity Material Material	|
	SolidBlock Material													deriving(Show)
		
-- A solid material that can make up a block.
	
data Material	=
	Air									|
	SolidColor Int Int Int			deriving(Show)
	
-- Gets one of the 8 child blocks of a larger block. The block to get is specified by the polarity
-- on each axis the block is away from the center.
subBlock	::	Block -> (Polarity, Polarity, Polarity) -> Block
subBlock (OctoBlock x) r								=	x !! (polIndex r)
subBlock (SlopedBlock a x l m) r						=	case axisOrder r a of
																		(_, maj, Negative)
																			|	maj == x		->	SolidBlock l
																			|	otherwise	->	SlopedBlock a x l m
																		(_, maj, Positive)
																			|	maj == x		->	SlopedBlock a x l m
																			|	otherwise	->	SolidBlock m
subBlock (CornerBlock x y z l m) (rx, ry, rz)	=	case length $ filter id $ [x == rx, y == ry, z == rz] of
																		3	->	SolidBlock l
																		2	->	CornerBlock x y z l m
																		_	->	SolidBlock m
subBlock (SolidBlock m) _								=	SolidBlock m

-- Creates a ball-like-thingy from a material
createBalloid	::	Material -> Block
createBalloid mat	=	OctoBlock $ map (\l -> case l of
							(x, y, z)	->	CornerBlock (negatePol x) (negatePol y) (negatePol z) mat Air) polList