-----------------------------------------------------------------------------
--
-- Module      :  MSnail.FRP
-- Copyright   :  (c) 2010, Dmitry Zamkov
-- License     :  BSD3 (See LICENSE)
--
-----------------------------------------------------------------------------

module MSnail.FRP (
	Time(..),
	DTime(..),
	tickRate,
	SF(..),
	InputStream(..),
	Event(..),
	writeIStream,
	readIStream,
	advanceReadIStream,
	advanceWriteIStream,
	predictSF,
	evalSF,
	optimizeSF,
	injectSF
) where

import qualified Control.Category (Category(..))
import Control.Arrow

-- Functionally reactive programming stuff. I wouldve used yampa for this had it
-- been more developed, but it includes many features I don't need and not many
-- that I do... I guess i'll just copy the basic interface of it here but create my
-- own tuned implementation.

-- Tick based time, independant of any measurable unit such as seconds. Negative ticks
-- on a signal can not be accessed.
type Time	=	Int

-- Measurement of a time interval in ticks. Where time represents a point in time, this will
-- represent the distance between those points.
type DTime	=	Int

-- Unless otherwise noted, this is how many ticks there are in a second.
tickRate		::	DTime
tickRate		=	10 ^ 4

-- A signal function which transforms a signal of one type to that of another, or modifies
-- it in some way. SF's must obey causaility, and may only use previous values of the input
-- signal when calculating a current value.
data SF a b	where
	EventSF				:: [(DTime, b)] -> SF a (Event b)
	TickCounterSF		::	Time -> SF a Time
	SwitchSF				::	Time -> SF a b -> SF a b -> SF a b
	InjectSF				::	Time -> SGen a -> SF a b -> SF a b
	UnionSF				::	SF a (Event b) -> SF a (Event b) -> SF a (Event b)
	RebaseSF				::	Time -> SF a b -> SF a b
	HoldSF				::	a -> SF (Event a) a
	AccumSF				::	b -> (a -> b -> b) -> SF (Event a) b
	ComposeSF			::	SF a c -> SF c b -> SF a b
	ArrSF 				::	(a -> b) -> SF a b
	ConstSF				::	b -> SF a b
	FirstSF				::	SF a b -> SF (a, c) (b, c)
	IdentitySF			::	SF a a
	
type SGen a	=	SF () a
	
-- A stream (event signal) that can accept new data to a point in the future, and have it
-- read from another point.
data InputStream a	=	InputStream	{
	iStreamSignal	::	SF () (Event a),
	writeTime		::	Time }
	
-- Writes an event to an input stream at its current write time.
writeIStream	:: a -> InputStream a -> InputStream a
writeIStream dat is	=	InputStream (UnionSF (EventSF [(writeTime is, dat)]) (iStreamSignal is)) (writeTime is)

-- Reads all information currently in an input stream.
readIStream		::	InputStream a -> SF () (Event a)
readIStream is	=	optimizeSF (iStreamSignal is)

-- Advances the read position on an input stream.
advanceReadIStream	::	DTime -> InputStream a -> InputStream a
advanceReadIStream t is	=	InputStream (RebaseSF t (iStreamSignal is)) (writeTime is + t)
	
-- Advances the write position on an input stream.
advanceWriteIStream	::	DTime -> InputStream a -> InputStream a
advanceWriteIStream t is	=	InputStream (iStreamSignal is) (writeTime is + t)
	
-- An event is a descrete occurence on a signal at a certain time. When there is no event at
-- a time, an empty list is returned. If multiple events occur on a single tick, all of them
-- are listed.
type Event a	=	[a]
	
instance Control.Category.Category SF where
     (.) 	=	flip ComposeSF
     id 		=	IdentitySF
	
instance Arrow SF where
	arr f									=	ArrSF f
	first l								=	FirstSF l
	
--	Injects some data from a signal generator into a signal function. After
-- the specified amount of data is feed into the sf, the new state of the
-- sf along with the output after the injection period are returned.
injectSF	::	SGen a -> Time -> SF a b -> (SF a b, b) 
injectSF	g t s =	(ns, evalSF (evalSF () ng) ns)
	where
		ng	=	optimizeSF (InjectSF t (ConstSF ()) g)
		ns	=	optimizeSF (InjectSF t g s)
	
-- Gets when the likely next change for a signal generator is.
predictSF	::	SGen a -> DTime
predictSF (EventSF ((0, _):r))	=	1
predictSF (EventSF ((x, _):r))	=	x
predictSF _								=	1

-- Gets the value of a signal function at its begining (0th tick)
evalSF		::	a -> SF a b -> b
evalSF _ (EventSF ((0, e):l))	=	e:(evalSF () (EventSF l))
evalSF _ (EventSF _)				=	[]
evalSF v (ComposeSF g h)		=	(evalSF (evalSF v g) h)
evalSF v (HoldSF l)				=	l
evalSF v (ArrSF f)				=	f v
evalSF _ (ConstSF x)				=	x
evalSF v (FirstSF f)				=	case v of (v, r) -> (evalSF v f, r)
evalSF v (IdentitySF)			=	v
evalSF _ (TickCounterSF s)		=	s
evalSF v (SwitchSF 0 _ f)		=	evalSF v f
evalSF v (SwitchSF _ f _)		=	evalSF v f

-- Improves the performance of a signal function with tree reduction.
optimizeSF	::	SF a b -> SF a b
optimizeSF (ComposeSF l m)		=	case (ComposeSF (optimizeSF l) (optimizeSF m)) of
		(ComposeSF _ (ConstSF c))					->	ConstSF c
		(ComposeSF _ (TickCounterSF t))			->	TickCounterSF t
		(ComposeSF (ConstSF c) (ArrSF f))		->	ConstSF (f c)
		(ComposeSF (IdentitySF) x)					->	x
		(ComposeSF x (IdentitySF))					->	x
		x													->	x
optimizeSF (InjectSF t g s)	=	case (InjectSF t (optimizeSF g) (optimizeSF s)) of
		(InjectSF t g (ComposeSF l m))			->	optimizeSF (ComposeSF 
																	(InjectSF t g l)
																	(InjectSF t (ComposeSF g l) m))
		(InjectSF t _ (ArrSF f))					->	ArrSF f
		(InjectSF t _ (ConstSF c))					->	ConstSF c
		(InjectSF t _ (IdentitySF))				->	IdentitySF
		(InjectSF t _ (FirstSF f))					->	FirstSF f
		(InjectSF t _ (TickCounterSF ti))		->	TickCounterSF (ti + t)
optimizeSF (SwitchSF t a b)	=	case (SwitchSF t (optimizeSF a) (optimizeSF b)) of
		x													->	x
optimizeSF (FirstSF k)			=	FirstSF (optimizeSF k)
optimizeSF x						=	x