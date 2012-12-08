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

module ExampleReduction2 (
    cReduction
) where

-- This file defines a reduction that can be tried with the visualization code.
--
-- The reduction is defined through makeDynamic from DynamicReduction.

import RuleAndSystem
import Term
import Reduction
import Omega
import DynamicReduction
import ExampleTermsAndSubstitutions
import ExampleRulesAndSystems

import Prelude

-- Reduction.
reduction' :: OmegaReduction Sigma Var System_a_f_x
reduction' = RCons (constructSequence terms) (constructSequence steps)
    where terms = rewriteSteps h_f_f_omega steps
          steps = zip ps rs
          ps = [2] : map (\p -> p ++ [1, 1]) ps
          rs = repeat rule_f_x_to_g_x

cReduction' :: CReduction Sigma Var System_a_f_x
cReduction' = CRCons reduction' (constructModulus phi)
    where phi n = n + 1

-- Conversion to a DynamicReduction.
cReduction :: DynamicReduction
cReduction = makeDynamic cReduction'
