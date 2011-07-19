{-
Copyright (C) 2011 Jeroen Ketema

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
    c_reduction
) where

-- This file defines a reduction that can be tried with the visualization code.
--
-- The reduction is defined through make_dynamic from DynamicReduction.

import RuleAndSystem
import Term
import Reduction
import Omega
import DynamicReduction
import ExampleTermsAndSubstitutions
import ExampleRulesAndSystems

-- A useful term.
h_f_f_omega :: Term_Sigma_Var
h_f_f_omega = function_term h [f_omega, f_omega]

-- The example reduction.
reduction' :: OmegaReduction Sigma Var System_a_f_x
reduction' = RCons (construct_sequence terms) (construct_sequence steps)
    where terms = rewrite_steps h_f_f_omega steps
          steps = zip ps rs
          ps = [2] : (map (\p -> p ++ [1, 1]) ps)
          rs = repeat rule_f_x_to_g_x

c_reduction' :: CReduction Sigma Var System_a_f_x
c_reduction' = CRCons reduction' (construct_modulus phi)
    where phi n = n + 1

-- Conversion to a DynamicReduction.
c_reduction :: DynamicReduction
c_reduction = make_dynamic c_reduction'
