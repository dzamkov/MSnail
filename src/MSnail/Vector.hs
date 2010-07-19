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
	PVector(..),
	negatePol,
	polIndex,
	polList,
	axisOrder,
	unitVector,
	pNumVector,
	addVector,
	multVector,
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
type PVector	=	Vector Polarity

-- Negates a polarity
negatePol	::	Polarity -> Polarity
negatePol Positive	=	Negative
negatePol Negative	=	Positive
	
-- Gets the index of a polarity vector
polIndex						::	PVector -> Int
polIndex (Vector l m n)	=	(pcount l * 4) + (pcount m * 2) + (pcount n)
	where 
		pcount Negative	=	0
		pcount Positive	=	1
		
polList		=	let	pols	=	[Negative, Positive]
							in	[(Vector x y z) | z <- pols, y <- pols, x <- pols]

-- Orders the axies in a vector so that the specified axis comes first.
axisOrder	::	Vector a -> Axis -> Vector a
axisOrder (Vector a b c) XAxis	=	(Vector a b c)
axisOrder (Vector a b c) YAxis	=	(Vector b c a)
axisOrder (Vector a b c) ZAxis	=	(Vector c a b)

-- Creates a unit vector
unitVector	::	(Num a) => Axis -> Polarity -> Vector a
unitVector (XAxis) (Positive)	=	Vector 1 0 0
unitVector (XAxis) (Negative)	=	Vector (-1) 0 0
unitVector (YAxis) (Positive)	=	Vector 0 1 0
unitVector (YAxis) (Negative)	=	Vector 0 (-1) 0
unitVector (ZAxis) (Positive)	=	Vector 0 0 1
unitVector (ZAxis) (Negative)	=	Vector 0 0 (-1)

-- Creates a num vector representation of a pvector
pNumVector	::	(Num a) => PVector -> Vector a
pNumVector	(Vector x y z)	=	Vector (f x) (f y) (f z)
	where
		f (Positive)	=	1
		f (Negative)	=	-1
		
-- Adds two vectors
addVector	::	(Num a) => Vector a -> Vector a -> Vector a
addVector (Vector ax ay az) (Vector bx by bz)	=	Vector (ax + bx) (ay + by) (az + bz)

-- Multiplies a vector by a scalar
multVector	::	(Num a) => Vector a -> a -> Vector a
multVector (Vector x y z) s	=	Vector (s * x) (s * y) (s * z)

-- Vector components
xUnits (Vector x _ _)	=	x
yUnits (Vector _ y _)	=	y
zUnits (Vector _ _ z)	=	z