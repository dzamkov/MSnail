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
	SF(..),
	evalSF,
	optimizeSF
) where

import qualified Control.Category (Category(..))
import Control.Arrow

-- Functionally reactive programming stuff. I wouldve used yampa for this had it
-- been more developed, but it includes many features I don't need and not many
-- that I do... I guess i'll just copy the basic interface of it here but create my
-- own tuned implementation.

-- Tick based time, independant of any measurable unit such as seconds. Negative ticks
-- on a signal can not be accessed.
type Time	=	Integer 

-- Measurement of a time interval in ticks. Where time represents a point in time, this will
-- represent the distance between those points.
type DTime	=	Integer

-- A signal function which transforms a signal of one type to that of another, or modifies
-- it in some way. SF's must obey causaility, and may only use previous values of the input
-- signal when calculating a current value.
data SF a b	where
	EventSF				:: [(DTime, b)] -> SF a (Event b)
	RebaseSF				::	Time -> SF a b -> SF a b
	HoldSF				::	a -> SF (Event a) a
	AccumSF				::	b -> (a -> b -> b) -> SF (Event a) b
	ComposeSF			::	SF a c -> SF c b -> SF a b
	ArrSF 				::	(a -> b) -> SF a b
	ConstSF				::	b -> SF a b
	FirstSF				::	SF a b -> SF (a, c) (b, c)
	IdentitySF			::	SF a a
	
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

-- Improves the performance of a signal function with tree reduction.
optimizeSF	::	SF a b -> SF a b
optimizeSF (ComposeSF l m)	=	case (ComposeSF (optimizeSF l) (optimizeSF m)) of
		(ComposeSF _ (ConstSF c))				->	ConstSF c
		(ComposeSF (ConstSF c) (ArrSF f))	->	ConstSF (f c)
		(ComposeSF (IdentitySF) x)				->	x
		(ComposeSF x (IdentitySF))				->	x
		x												->	x
optimizeSF (RebaseSF t i)	=	case (RebaseSF t (optimizeSF i)) of
		(RebaseSF t (EventSF es))				->	EventSF (fst $ evs (es, Nothing) t)
		(RebaseSF _ (ConstSF c))				->	ConstSF c
		(RebaseSF t (ComposeSF l m))			->	case (l, m) of
			(EventSF es, HoldSF st)					->	case (evs (es, Just st) t) of
				(nes, (Just nst))							->	ComposeSF (EventSF nes) (HoldSF nst)
			(l, m)										->	optimizeSF (ComposeSF 
																	(RebaseSF t l) 
																	(RebaseSF t m))
		x												->	x
	where
		evs	::	([(DTime, a)], Maybe a) -> Time -> ([(DTime, a)], Maybe a)
		evs ((et, ei):r, l) t
			|	t > et		=	evs (r, Just ei) (t - et)
			|	otherwise	=	((et - t, ei):r, l)
		evs ([], l) _		=	([], l)
optimizeSF x					=	x