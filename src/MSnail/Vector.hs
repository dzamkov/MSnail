-----------------------------------------------------------------------------
--
-- Module      :  MSnail.Vector
-- Copyright   :  (c) 2010, Dmitry Zamkov
-- License     :  BSD3 (See LICENSE)
--
-----------------------------------------------------------------------------

module MSnail.Vector (
	Axis(..),
	Polarity(..),
	Vector(..),
	DVector(..),
	FVector(..),
	IVector(..),
	negatePol,
	polIndex,
	polList,
	axisOrder,
	unitVector,
	xUnits,
	yUnits,
	zUnits
) where

-- One of the three orthagonal Axies.
data Axis	=
	XAxis			|
	YAxis			|
	ZAxis			deriving(Show, Eq, Ord)
	
-- Positive or negative
data Polarity	=
	Positive		|
	Negative		deriving(Show, Eq, Ord)
	
--	Direction and magnitude in 3d space.
data Vector a	=	Vector a a a	deriving(Show, Eq, Ord)
type DVector	=	Vector Double
type FVector	=	Vector Float
type IVector	=	Vector Int

-- Negates a polarity
negatePol	::	Polarity -> Polarity
negatePol Positive	=	Negative
negatePol Negative	=	Positive
	
-- Gets the index of a tuple of polarities
polIndex		::	(Polarity, Polarity, Polarity) -> Int
polIndex (l, m, n)	=	(pcount l * 4) + (pcount m * 2) + (pcount n)
	where 
		pcount Negative	=	0
		pcount Positive	=	1
		
polList		=	let	pols	=	[Negative, Positive]
							in	[(x, y, z) | z <- pols, y <- pols, x <- pols]

-- Orders the axies in a tuple so that the specified axis comes first.
axisOrder	::	(Polarity, Polarity, Polarity) -> Axis -> (Polarity, Polarity, Polarity)
axisOrder (a, b, c) XAxis	=	(a, b, c)
axisOrder (a, b, c) YAxis	=	(b, c, a)
axisOrder (a, b, c) ZAxis	=	(c, a, b)

-- Creates a unit vector
unitVector	::	(Num a) => Axis -> Polarity -> Vector a
unitVector (XAxis) (Positive)	=	Vector 1 0 0
unitVector (XAxis) (Negative)	=	Vector (-1) 0 0
unitVector (YAxis) (Positive)	=	Vector 0 1 0
unitVector (YAxis) (Negative)	=	Vector 0 (-1) 0
unitVector (ZAxis) (Positive)	=	Vector 0 0 1
unitVector (ZAxis) (Negative)	=	Vector 0 0 (-1)

-- Vector components
xUnits (Vector x _ _)	=	x
yUnits (Vector _ y _)	=	y
zUnits (Vector _ _ z)	=	z