{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module ExampleReduction (
    c_reduction
) where

import RuleAndSystem
import Term
import Reduction
import Omega
import DynamicReduction

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
f_x = function_term f [Variable x]

g_x :: Term DynamicSigma DynamicVar
g_x = function_term g [Variable x]

f_omega :: Term DynamicSigma DynamicVar
f_omega = function_term f [f_omega]

k_3_omega :: Term DynamicSigma DynamicVar
k_3_omega = function_term k_3 [k_3_omega, k_3_omega, k_3_omega]

k_4_omega :: Term DynamicSigma DynamicVar
k_4_omega = function_term k_4 [k_4_omega, k_3_omega, f_omega, k_4_omega]

k_5_omega :: Term DynamicSigma DynamicVar
k_5_omega = function_term k_5 [k_5_omega, k_4_omega, k_3_omega, f_omega,
                               k_5_omega]

h_f_f_omega :: Term DynamicSigma DynamicVar
h_f_f_omega = function_term h [k_5_omega, f_omega]

rule_f_x_to_g_x :: RewriteRule DynamicSigma DynamicVar
rule_f_x_to_g_x = Rule f_x g_x

reduction :: OmegaReduction DynamicSigma DynamicVar DynamicSystem
reduction = RCons (construct_sequence terms) (construct_sequence steps)
    where terms = rewrite_steps h_f_f_omega steps
          steps = zip ps rs
          ps = [2] : (map (\p -> p ++ [1, 1]) ps)
          rs = repeat rule_f_x_to_g_x

c_reduction :: CReduction DynamicSigma DynamicVar DynamicSystem
c_reduction = CRCons reduction (construct_modulus phi)
    where phi x = x + 1
