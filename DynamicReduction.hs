{-
Copyright (C) 2011, 2012 Jeroen Ketema

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

-- This module defines a "dynamic" reduction type.
--
-- Using make_dynamic, any reduction of length at most omega can be converted
-- into a reduction of the type defined here.

module DynamicReduction (
    DynamicSigma(DynamicFun),
    DynamicVar(DynamicVar),
    DynamicSystem,
    DynamicReduction,
    dynamicSystem,
    makeDynamic
) where

import SignatureAndVariables
import Term
import RuleAndSystem
import SystemOfNotation
import Reduction
import Omega

import Prelude

-- A "dynamic" signature: the arity is encoded in the function symbol, where
-- the actual function symbol is represented by a string.
data DynamicSigma = DynamicFun String Int

instance Show DynamicSigma where
    show (DynamicFun f _) = f

instance Eq DynamicSigma where
    (DynamicFun f a) == (DynamicFun g b) = (f == g) && (a == b)

instance Signature DynamicSigma where
    arity (DynamicFun _ a) = a

-- A "dynamic" variable set: variables are represented by strings.
data DynamicVar = DynamicVar String

instance Show DynamicVar where
    show (DynamicVar x) = x

instance Eq DynamicVar where
    (DynamicVar x) == (DynamicVar y) = x == y

instance Variables DynamicVar

-- A "dynamic" rewrite system: the rules are simply not known.
--
-- Remark that not knowing the rules still makes sense in the context of the
-- confluence and compression algorithms, as these use the rules embedded in
-- the reductions they are applied to.
type DynamicSystem = System DynamicSigma DynamicVar

dynamicSystem :: DynamicSystem
dynamicSystem = SystemCons rs
    where rs = error "Rules cannot be queried in dynamic systems"

-- A "dynamic" reduction.
type DynamicReduction= CReduction DynamicSigma DynamicVar DynamicSystem

-- The following converts arbitrary reductions of length at most omega to
-- "dynamic" reductions. The main function is makeDynamic.
dynamicTerm :: (Show s, Show v, Signature s, Variables v)
    => Term s v -> Term DynamicSigma DynamicVar
dynamicTerm (Function f xs) = Function f' xs'
    where f'  = DynamicFun (show f) (arity f)
          xs' = fmap dynamicTerm xs
dynamicTerm (Variable x) = Variable x'
    where x' = DynamicVar (show x)

dynamicTerms :: (Show s, Show v, RewriteSystem s v r)
    => CReduction s v r -> [Term DynamicSigma DynamicVar]
dynamicTerms (CRCons (RCons ts _) _) = map dynamicTerm (getFrom ts ordZero)

dynamicStep :: (Show s, Show v, Signature s, Variables v)
    => Step s v -> Step DynamicSigma DynamicVar
dynamicStep (ps, Rule l r) = (ps, Rule l' r')
    where l' = dynamicTerm l
          r' = dynamicTerm r

dynamicSteps :: (Show s, Show v, RewriteSystem s v r)
    => CReduction s v r -> [Step DynamicSigma DynamicVar]
dynamicSteps (CRCons (RCons _ ss) _) = map dynamicStep (getFrom ss ordZero)

dynamicModulus :: RewriteSystem s v r
    => CReduction s v r -> Modulus Omega
dynamicModulus (CRCons _ phi) = constructModulus phi'
    where phi' depth = ord2Int (phi ordZero depth)

makeDynamic :: (Show s, Show v, RewriteSystem s v r)
    => CReduction s v r -> DynamicReduction
makeDynamic reduction
    | atMostLengthOmega reduction = CRCons (RCons ts ss) phi
    | otherwise                   = error "Reduction too long"
        where ts    = constructSequence terms
              ss    = constructSequence steps
              phi   = dynamicModulus reduction
              terms = dynamicTerms reduction
              steps = dynamicSteps reduction
