module ExampleReduction (
    c_reduction
) where

import RuleAndSystem
import Term
import OmegaReduction
import DynamicOmegaReduction

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

f_x :: Term DynamicSigma DynamicVar
f_x = function_term f [(1, Variable x)]

g_x :: Term DynamicSigma DynamicVar
g_x = function_term g [(1, Variable x)]

f_omega :: Term DynamicSigma DynamicVar
f_omega = function_term f [(1, f_omega)]

k_3_omega :: Term DynamicSigma DynamicVar
k_3_omega = function_term k_3 [(1, k_3_omega), (2, k_3_omega), (3, k_3_omega)]

k_4_omega :: Term DynamicSigma DynamicVar
k_4_omega = function_term k_4 [(1, k_4_omega), (2, k_3_omega), (3, f_omega),
                               (4, k_4_omega)]

k_5_omega :: Term DynamicSigma DynamicVar
k_5_omega = function_term k_5 [(1, k_5_omega), (2, k_4_omega), (3, k_3_omega),
                               (4, f_omega), (5, k_5_omega)]

h_f_f_omega :: Term DynamicSigma DynamicVar
h_f_f_omega = function_term h [(1, k_5_omega), (2, f_omega)]

rule_f_x_to_g_x :: RewriteRule DynamicSigma DynamicVar
rule_f_x_to_g_x = Rule f_x g_x

reduction :: Reduction DynamicSigma DynamicVar DynamicSystem
reduction = RCons ts (zip ps rs)
    where ps = [2]:(map (\p -> p ++ [1, 1]) ps)
          rs = rule_f_x_to_g_x:rs
          ts = rewrite_steps h_f_f_omega (zip ps rs)

c_reduction :: CReduction DynamicSigma DynamicVar DynamicSystem
c_reduction = CRCons reduction (\n -> succ n)
