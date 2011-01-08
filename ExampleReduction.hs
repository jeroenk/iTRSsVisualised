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

x :: DynamicVar
x = DynamicVar "x"

f_x :: Term DynamicSigma DynamicVar
f_x = function_term f [(1, Variable x)]

g_x :: Term DynamicSigma DynamicVar
g_x = function_term g [(1, Variable x)]

f_omega :: Term DynamicSigma DynamicVar
f_omega = function_term f [(1, f_omega)]

h_f_f_omega :: Term DynamicSigma DynamicVar
h_f_f_omega = function_term h [(1, f_omega), (2, f_omega)]

rule_f_x_to_g_x :: RewriteRule DynamicSigma DynamicVar
rule_f_x_to_g_x = Rule f_x g_x

reduction :: Reduction DynamicSigma DynamicVar DynamicSystem
reduction = RCons ts (zip ps rs)
    where ps = [2]:(map (\p -> p ++ [1, 1]) ps)
          rs = rule_f_x_to_g_x:rs
          ts = rewrite_steps h_f_f_omega (zip ps rs)

c_reduction :: CReduction DynamicSigma DynamicVar DynamicSystem
c_reduction = CRCons reduction (\x -> succ x)
