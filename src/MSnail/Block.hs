-----------------------------------------------------------------------------
--
-- Module      :  MSnail.Block
-- Copyright   :  (c) 2010, Dmitry Zamkov
-- License     :  BSD3 (See LICENSE)
--
-----------------------------------------------------------------------------

module MSnail.Block (
	Block(..),
	BlockSurface(..),
	Material(..),
	subBlock,
	surfaceDecompose
) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import MSnail.Vector

-- Represents a finite cubic region made from materials. Blocks have no real-world size
-- and all information about the blocks contents must be specified in the block itself. Blocks
-- have both visual and physical properties.

data Block	=
	OctoBlock [Block]				|
	SolidBlock Material			deriving(Show)
	
-- Describes a surface of a block. 
	
data BlockSurface		=
	BlockSurface Axis Polarity Material Material deriving(Show)
		
-- A solid material that can make up a block.
	
data Material	=
	Air										|
	SolidColor Double Double Double	deriving(Show)
	
-- Gets one of the 8 child blocks of a larger block. The block to get is specified by the polarity
-- on each axis the block is away from the center.
subBlock	::	Block -> PVector -> Block
subBlock (OctoBlock x) r								=	x !! (polIndex r)
subBlock (SolidBlock m) _								=	SolidBlock m

-- Creates quads for the surfaces inside a block (Not on the faces of the block itself, just interior surfaces). Also, notice
-- how complicated this type is. Lemme explain. "a" represents a frame of reference, or a coordinate system. The first specified "a"
-- shows the frame of reference for the first specified block. The function of "a" can be used to get the frame of reference for a block
-- that is the child of the current frame of reference. "b" is a standard accumulator for a list/set/bundle of triangles. Each time a new
-- triangle is detected, the accumlator function is called with the frame of reference, details about the triangle and the origin of the triangle.
-- A last function specifies which pairs of bordering materials should be included as triangles.

surfaceDecompose	::	Block -> 
							(a -> PVector -> a) ->
							a ->
							(b -> a -> BlockSurface -> b) ->
							b ->
							(Material -> Material -> Bool) ->
							b
surfaceDecompose block@(OctoBlock blocks) af ac bf bc mf		=	res
	where
		polblocks	=	zipWith (\l m -> (l, m)) blocks polList
		nb				=	foldl (\curb blockpol -> case blockpol of
								(block, pol)	->	let	cura	=	af ac pol
																in	surfaceDecompose block af cura bf curb mf
							) bc polblocks
							
		inblocks af ac bf bc mf (SolidBlock pm) (SolidBlock nm) axis	=	case mf pm nm of
				(True)	->	bf bc ac (BlockSurface axis Negative pm nm)
				(False)	->	bc
		inblocks af ac bf bc mf pblock nblock axis							=	foldl (\c it -> case it of
					(psubi, nsubi)	->	inblocks af (af ac psubi) bf c mf (subBlock pblock psubi) (subBlock nblock nsubi) axis
					) bc
				[(axisUnorder (Vector Negative x y) axis, axisUnorder (Vector Positive x y) axis)
					| x <- [Positive, Negative], y <- [Positive, Negative]]

		res	=	foldl (\c it -> case it of
						(psubi, nsubi, axis)	->	inblocks af (af ac psubi) bf c mf (subBlock block psubi) (subBlock block nsubi) axis
					) nb [(axisUnorder (Vector Positive x y) z, axisUnorder (Vector Negative x y) z, z) 
						| x <- [Positive, Negative], y <- [Positive, Negative], z <- [XAxis, YAxis, ZAxis]]
surfaceDecompose (SolidBlock _) af ac bf bc mf			=	bc