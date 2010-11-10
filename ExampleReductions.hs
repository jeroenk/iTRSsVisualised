module ExampleReductions (
    c_red_1
) where

import RulesAndSystems
import OmegaReductions
import ExampleTermsAndSubstitutions
import ExampleRulesAndSystems

red_1 :: Reduction Sigma Var System_a_f_x
red_1 = RConst ts (zip ps rs)
    where ps = []:(map (\p -> 1:1:p) ps)
          rs = rule_f_x_to_g_x:rs
          ts = rewrite_steps f_omega (zip ps rs)

c_red_1 :: CReduction Sigma Var System_a_f_x
c_red_1 = CRConst red_1 (\x -> succ x)
