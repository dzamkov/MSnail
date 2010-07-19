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
	createBalloid,
	surfaceDecompose
) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import MSnail.Vector

-- Represents a finite cubic region made from materials. Blocks have no real-world size
-- and all information about the blocks contents must be specified in the block itself. Blocks
-- have both visual and physical properties.

data Block	=
	OctoBlock [Block]												|
	SlopedBlock	Axis Polarity Material Material			|
	CornerBlock PVector Material Material					|
	SolidBlock Material													deriving(Show)
	
-- Describes a surface of a block
	
data BlockSurface		=
	FaceSurface Axis Polarity		|
	InteriorSurface 					deriving(Show)
	
-- A triangle created during a triangle decomposition of a blocks surface.
	
data BlockTriangle	=
	BlockTriangle PVector PVector PVector Material Material	deriving(Show)
		
-- A solid material that can make up a block.
	
data Material	=
	Air									|
	SolidColor Int Int Int			deriving(Show)
	
-- Gets one of the 8 child blocks of a larger block. The block to get is specified by the polarity
-- on each axis the block is away from the center.
subBlock	::	Block -> PVector -> Block
subBlock (OctoBlock x) r								=	x !! (polIndex r)
subBlock (SlopedBlock a x l m) r						=	case axisOrder r a of
																		(Vector _ maj Negative)
																			|	maj == x		->	SolidBlock l
																			|	otherwise	->	SlopedBlock a x l m
																		(Vector _ maj Positive)
																			|	maj == x		->	SlopedBlock a x l m
																			|	otherwise	->	SolidBlock m
subBlock (CornerBlock (Vector x y z) l m) (Vector rx ry rz)	=	
																	(case length $ filter id $ [x == rx, y == ry, z == rz] of
																		3	->	SolidBlock l
																		2	->	CornerBlock (Vector x y z) l m
																		_	->	SolidBlock m)
subBlock (SolidBlock m) _								=	SolidBlock m

-- Creates a ball-like-thingy from a material
createBalloid	::	Material -> Block
createBalloid mat	=	OctoBlock $ map (\l -> case l of
							(Vector x y z)	->	CornerBlock (Vector (negatePol x) (negatePol y) (negatePol z)) mat Air) polList
						
-- Creates triangles for the surfaces inside a block (Not on the faces of the block itself, just interior surfaces). Also, notice
-- how complicated this type is. Lemme explain. "a" represents a frame of reference, or a coordinate system. The first specified "a"
-- shows the frame of reference for the first specified block. The function of "a" can be used to get the frame of reference for a block
-- that is the child of the current frame of reference. "b" is a standard accumulator for a list/set/bundle of triangles. Each time a new
-- triangle is detected, the accumlator function is called with the frame of reference, details about the triangle and the origin of the triangle.
-- A last function specifies which pairs of bordering materials should be included as triangles.

surfaceDecompose	::	Block -> 
							(a -> PVector -> a) ->
							a ->
							(b -> a -> BlockTriangle -> BlockSurface -> b) ->
							b ->
							(Material -> Material -> Bool) ->
							b
surfaceDecompose (OctoBlock blocks) af ac bf bc mf		=	res
	where
		polblocks	=	zipWith (\l m -> (l, m)) blocks polList
		nb				=	foldl (\curb blockpol -> case blockpol of
								(block, pol)	->	let	cura	=	af ac pol
																in	surfaceDecompose block af cura bf curb mf
							) bc polblocks
							
		-- TODO: Face surfaces of interior blocks
							
		res			=	nb
surfaceDecompose (CornerBlock c f b) af ac bf bc mf	=	case mf f b of
								(True)	->	bf bc ac (case c of
									(Vector x y z)	->	BlockTriangle
										(Vector (negatePol x) y z)
										(Vector x (negatePol y) z)
										(Vector x y (negatePol z))
										f b) InteriorSurface
								(False)	->	bc