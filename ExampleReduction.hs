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

module ExampleReduction (
    cReduction
) where

-- This file defines a reduction that can be tried with the visualization code.
--
-- The reduction explicitly defines all its elements as dynamic (not using
-- makeDynamic from DynamicReduction).

import RuleAndSystem
import Term
import Reduction
import Omega
import DynamicReduction

import Prelude

-- Signature and variables.
f :: DynamicSigma
f = DynamicFun "f" 1

g :: DynamicSigma
g = DynamicFun "g" 1

h :: DynamicSigma
h = DynamicFun "h" 2

k_3 :: DynamicSigma
k_3 = DynamicFun "k3" 3

k_4 :: DynamicSigma
k_4 = DynamicFun "k4" 4

k_5 :: DynamicSigma
k_5 = DynamicFun "k5" 5

x :: DynamicVar
x = DynamicVar "x"

-- Terms.
f_x :: Term DynamicSigma DynamicVar
f_x = functionTerm f [Variable x]

g_x :: Term DynamicSigma DynamicVar
g_x = functionTerm g [Variable x]

f_omega :: Term DynamicSigma DynamicVar
f_omega = functionTerm f [f_omega]

k_3_omega :: Term DynamicSigma DynamicVar
k_3_omega = functionTerm k_3 [k_3_omega, k_3_omega, k_3_omega]

k_4_omega :: Term DynamicSigma DynamicVar
k_4_omega = functionTerm k_4 [k_4_omega, k_3_omega, f_omega, k_4_omega]

k_5_omega :: Term DynamicSigma DynamicVar
k_5_omega = functionTerm k_5 [k_5_omega, k_4_omega, k_3_omega, f_omega,
                                  k_5_omega]

h_f_f_omega :: Term DynamicSigma DynamicVar
h_f_f_omega = functionTerm h [k_5_omega, f_omega]

-- Rewrite rule.
rule_f_x_to_g_x :: RewriteRule DynamicSigma DynamicVar
rule_f_x_to_g_x = Rule f_x g_x

-- Reduction.
reduction :: OmegaReduction DynamicSigma DynamicVar DynamicSystem
reduction = RCons (constructSequence terms) (constructSequence steps)
    where terms = rewriteSteps h_f_f_omega steps
          steps = zip ps rs
          ps = [2] : map (\p -> p ++ [1, 1]) ps
          rs = repeat rule_f_x_to_g_x

cReduction :: DynamicReduction
cReduction = CRCons reduction (constructModulus phi)
    where phi n = n + 1
