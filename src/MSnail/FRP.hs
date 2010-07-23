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
	SF,
	Event(..),
	SGen(..),
	composeSF,
	arrSF,
	constSF,
	firstSF,
	identitySF,
	returnSF,
	accumSF,
	injectSF,
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
type Time	=	Int

-- Measurement of a time interval in ticks. Where time represents a point in time, this will
-- represent the distance between those points.
type DTime	=	Int

-- A signal function which transforms a signal of one type to that of another, or modifies
-- it in some way. SF's must obey causaility, and may only use previous values of the input
-- signal when calculating a current value.
data SF a b	where
	ComposeSF			::	SF a c -> SF c b -> SF a b
	ArrSF 				::	(a -> b) -> SF a b
	ConstSF				::	b -> SF a b
	FirstSF				::	SF a b -> SF (a, c) (b, c)
	IdentitySF			::	SF a a
	AccumSF				::	b -> (a -> b -> b) -> SF a b
	
composeSF	=	ComposeSF
arrSF			=	ArrSF
constSF		=	ConstSF
firstSF		=	FirstSF
identitySF	=	IdentitySF
returnSF		=	IdentitySF
accumSF		=	AccumSF
	
type SGen a	=	SF () a
	
-- An event is a descrete occurence on a signal at a certain time. When there is no event at
-- a time, an empty list is returned. If multiple events occur on a single tick, all of them
-- are listed.
type Event a	=	[a]
	
instance Control.Category.Category SF where
     (.) 	=	flip composeSF
     id 		=	identitySF
	
instance Arrow SF where
	arr f									=	arrSF f
	first l								=	firstSF l
	
-- Injects a tick of data into a signal function and returns the next state of the signal function
-- along with the processed data.
injectSF		::	a -> SF a b -> (SF a b, b)
injectSF	d (ComposeSF l m)	=	(ComposeSF (fst lres) (fst mres), snd mres)
	where
		lres	=	injectSF d l
		mres	=	injectSF (snd lres) m
injectSF d (IdentitySF)		=	(IdentitySF, d)
injectSF d (ArrSF f)			=	(ArrSF f, f d)
injectSF d (FirstSF f)		=	(FirstSF (fst fres), (snd fres, snd d))
	where
		fres	=	injectSF (fst d) f
injectSF d (ConstSF c)		=	(ConstSF c, c)
injectSF d (AccumSF c f)	=	(AccumSF ns f, ns)
	where
		ns	=	f d c
		
-- Optimizes a signal function.
optimizeSF	::	SF a b -> SF a b
optimizeSF x	=	x