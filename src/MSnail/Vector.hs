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
	octantPol,
	axisOrder,
	axisUnorder,
	unitVector,
	pNumVector,
	addVector,
	multVector,
	toVertex3,
	toVector3,
	xUnits,
	yUnits,
	zUnits
) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

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
polIndex (Vector l m n)	=	(pcount l) + (pcount m * 2) + (pcount n * 4)
	where 
		pcount Negative	=	0
		pcount Positive	=	1
		
polList		=	let	pols	=	[Negative, Positive]
							in	[(Vector x y z) | z <- pols, y <- pols, x <- pols]
					
-- Gets the polarity of the octant a pvector is in. An octant is positive if and only if
-- it has an odd number of positive components. This is useful in determining which way
-- triangles should face.					
octantPol	::	PVector -> Polarity
octantPol (Vector x y z)	=	x `mult` y `mult` z
	where
		mult l (Negative) =	negatePol l
		mult l (Positive)	=	l

-- Orders the axies in a vector so that the specified axis comes first.
axisOrder	::	Vector a -> Axis -> Vector a
axisOrder (Vector a b c) XAxis	=	(Vector a b c)
axisOrder (Vector a b c) YAxis	=	(Vector b c a)
axisOrder (Vector a b c) ZAxis	=	(Vector c a b)

axisUnorder	::	Vector a -> Axis -> Vector a
axisUnorder	(Vector a b c) XAxis	=	(Vector a b c)
axisUnorder (Vector b c a) YAxis	=	(Vector a b c)
axisUnorder (Vector c a b) ZAxis	=	(Vector a b c)

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

-- To opengl conversions
toVertex3	::	Vector a -> Vertex3 a
toVertex3 (Vector x y z)	=	Vertex3 x y z
toVector3	::	Vector a -> Vector3 a
toVector3 (Vector x y z)	=	Vector3 x y z

-- Vector components
xUnits (Vector x _ _)	=	x
yUnits (Vector _ y _)	=	y
zUnits (Vector _ _ z)	=	z