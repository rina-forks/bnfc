{-# LANGUAGE DeriveFunctor #-}

{-
    BNF Converter: TreeSitter Grammar Generator
    Copyright (C) 2004  Author:  Markus Forsberg, Michael Pellauer,
                                 Bjorn Bringert

    Description   : This module identifies and transforms rules which match
                    the empty string, as required by Treesitter.

    Author        : Kangjing Huang (huangkangjing@gmail.com)
    Created       : 23 Nov, 2023

-}
module BNFC.Backend.TreeSitter.MatchesEmpty where

import BNFC.CF(SentForm, Cat, Rule, rhsRule)

import qualified Data.Maybe as Maybe
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Debug.Trace as Trace

-- | A single non-terminal ('Cat') or terminal token name ('String').
-- A 'SentForm' is a list of these 'SentSym's.
type SentSym = Either Cat String

-- | Possibly empty non-terminals (Cat) or terminals (String, as token name).
type KnownEmpty = Set.Set SentSym
data Optional = Optional | NonOptional deriving (Eq, Show)

type OptionalSentSym = (Optional, SentSym)
type OptionalSentForm = [OptionalSentSym]

data MatchesEmpty a = MatchesEmpty a | NonEmpty a deriving (Eq, Show, Functor)

matchesEmpty (MatchesEmpty _) = True
matchesEmpty (NonEmpty _) = False

unMatchesEmpty (MatchesEmpty x) = x
unMatchesEmpty (NonEmpty x) = x

seqMatchesEmpty :: Semigroup a => MatchesEmpty a -> MatchesEmpty a -> MatchesEmpty a
seqMatchesEmpty (MatchesEmpty x) (MatchesEmpty y) = MatchesEmpty (x <> y)
seqMatchesEmpty               x                y  = NonEmpty (unMatchesEmpty x <> unMatchesEmpty y)

seqListMatchesEmpty :: Monoid a => [MatchesEmpty a] -> MatchesEmpty a
seqListMatchesEmpty = foldr seqMatchesEmpty (MatchesEmpty mempty)

choiceMatchesEmpty :: Semigroup a => MatchesEmpty a -> MatchesEmpty a -> MatchesEmpty a
choiceMatchesEmpty (NonEmpty x) (NonEmpty y) = NonEmpty (x <> y)
choiceMatchesEmpty           x            y  = MatchesEmpty (unMatchesEmpty x <> unMatchesEmpty y)

choiceListMatchesEmpty :: Monoid a => [MatchesEmpty a] -> MatchesEmpty a
choiceListMatchesEmpty = foldr choiceMatchesEmpty (NonEmpty mempty)

-- TODO: if we have a rule like X ::= "" | "A", then we need to go Left []
-- for the first one to indicate that it DOES match empty and indicate that
-- there is nothing remaining if empty is removed.

possiblyEmptySentSym :: KnownEmpty -> SentSym -> MatchesEmpty OptionalSentSym
possiblyEmptySentSym knownEmpty sym =
  if sym `Set.member` knownEmpty then
    MatchesEmpty (Optional, sym)
  else
    NonEmpty (NonOptional, sym)

possiblyEmptyRule :: KnownEmpty -> SentForm -> MatchesEmpty [OptionalSentForm]
possiblyEmptyRule knownEmpty sent =
  case seqListMatchesEmpty sent' of
    MatchesEmpty syms -> MatchesEmpty $ Maybe.mapMaybe headNonOptional (List.tails syms)
    NonEmpty syms -> NonEmpty [syms]
  where
    sent' = map (fmap pure . possiblyEmptySentSym knownEmpty) sent

    headNonOptional ((_,x):xs) = Just ((NonOptional, x) : xs)
    headNonOptional [] = Nothing

-- | Returns whether the given Cat with the given Rules could match the empty
--   string, given the set of currently-known empty things.
possiblyEmptyCat :: KnownEmpty -> (Cat, [Rule]) -> MatchesEmpty [OptionalSentForm]
possiblyEmptyCat knownEmpty (_, rules) =
  choiceListMatchesEmpty $ map (possiblyEmptyRule knownEmpty . rhsRule) rules

possiblyEmptyCats :: [(Cat, [Rule])] -> KnownEmpty -> KnownEmpty
possiblyEmptyCats cats knownEmpty =
  Set.fromList (map (Left . fst) newEmpties)
    `Set.union` knownEmpty
  where
    newEmpties = filter (matchesEmpty . possiblyEmptyCat knownEmpty) cats

fixPointKnownEmpty :: [(Cat, [Rule])] -> KnownEmpty
fixPointKnownEmpty cats =
  case fixPoint of
    Just knownEmpty -> Trace.traceShowId knownEmpty
    Nothing -> error "impossible due to fix point iteration"
  where
    knownEmptySeq = iterate (possiblyEmptyCats cats) Set.empty

    fixPoint = fst <$> List.find (uncurry (==)) (knownEmptySeq `zip` drop 1 knownEmptySeq)

fixSentence :: KnownEmpty -> SentForm -> [OptionalSentForm]
fixSentence knownEmpty = unMatchesEmpty . possiblyEmptyRule knownEmpty

