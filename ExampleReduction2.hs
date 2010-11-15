module ExampleReduction2 (
    c_reduction
) where

import MyShow
import RulesAndSystems
import Terms
import OmegaReductions
import DynamicOmegaReductions
import ExampleTermsAndSubstitutions
import ExampleRulesAndSystems

instance MyShow Char where
    myshow x = [x]

h_f_f_omega :: Term_Sigma_Var
h_f_f_omega = function_term 'h' [(1, f_omega), (2, f_omega)]

reduction' :: Reduction Sigma Var System_a_f_x
reduction' = RConst ts (zip ps rs)
    where ps = [2]:(map (\p -> p ++ [1, 1]) ps)
          rs = rule_f_x_to_g_x:rs
          ts = rewrite_steps h_f_f_omega (zip ps rs)

c_reduction' :: CReduction Sigma Var System_a_f_x
c_reduction' = CRConst reduction' (\x -> x + 1)

c_reduction :: CReduction DynamicSigma DynamicVar DynamicSystem
c_reduction = make_omega_dynamic c_reduction'
