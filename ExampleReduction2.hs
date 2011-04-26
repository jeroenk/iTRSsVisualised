module ExampleReduction2 (
    c_reduction
) where

import RuleAndSystem
import Term
import Reduction
import Omega
import DynamicReduction
import ExampleTermsAndSubstitutions
import ExampleRulesAndSystems

h_f_f_omega :: Term_Sigma_Var
h_f_f_omega = function_term h [f_omega, f_omega]

reduction' :: OmegaReduction Sigma Var System_a_f_x
reduction' = RCons (construct_sequence terms) (construct_sequence steps)
    where terms = rewrite_steps h_f_f_omega steps
          steps = zip ps rs
          ps = [2] : (map (\p -> p ++ [1, 1]) ps)
          rs = repeat rule_f_x_to_g_x

c_reduction' :: CReduction Sigma Var System_a_f_x
c_reduction' = CRCons reduction' (construct_modulus phi)
    where phi x = x + 1

c_reduction :: CReduction DynamicSigma DynamicVar DynamicSystem
c_reduction = make_dynamic c_reduction'
