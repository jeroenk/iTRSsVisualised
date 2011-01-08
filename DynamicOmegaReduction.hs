{-# LANGUAGE MultiParamTypeClasses #-}

module DynamicOmegaReduction (
    DynamicSigma(DynamicFun),
    DynamicVar(DynamicVar),
    DynamicSystem,
    make_omega_dynamic,
    make_omega_trans_dynamic
) where

import SignatureAndVariables
import Term
import RuleAndSystem
import OmegaReduction
import SystemOfNotation
import qualified TransfiniteReduction as T

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

-- A "dynamic" rewrite system: the rules are simply not know. This still makes
-- sense in the context of the confluence and compression algorithms, as those
-- use the rules embedded in the reductions they are applied to.
data DynamicSystem = DynamicSystem

instance RewriteSystem DynamicSigma DynamicVar DynamicSystem where
    rules DynamicSystem = error "Rules cannot be queried in dynamic systems"

make_term_dynamic :: (Show s, Show v, Signature s, Variables v)
    => Term s v -> Term DynamicSigma DynamicVar
make_term_dynamic (Function f ss) = Function f' ss'
    where f'  = DynamicFun (show f) a
          ss' = fmap make_term_dynamic ss
          a   = arity f
make_term_dynamic (Variable x)    = Variable x'
    where x' = DynamicVar (show x)

make_terms_dynamic :: (Show s, Show v, Signature s, Variables v)
    => [Term s v] -> [Term DynamicSigma DynamicVar]
make_terms_dynamic ts = map make_term_dynamic ts

make_step_dynamic :: (Show s, Show v, Signature s, Variables v)
    => Step s v -> Step DynamicSigma DynamicVar
make_step_dynamic (ps, Rule l r) = (ps, Rule l' r')
    where l' = make_term_dynamic l
          r' = make_term_dynamic r

make_steps_dynamic :: (Show s, Show v, Signature s, Variables v)
    => [Step s v] -> [Step DynamicSigma DynamicVar]
make_steps_dynamic ss = map make_step_dynamic ss

-- Construct a "dynamic" reduction of length at most omega.
make_omega_dynamic :: (Show s, Show v,
                       Signature s, Variables v, RewriteSystem s v r)
    => CReduction s v r -> CReduction DynamicSigma DynamicVar DynamicSystem
make_omega_dynamic (CRCons (RCons ts ss) phi)
    = CRCons (RCons ts' ss') phi
    where ts' = make_terms_dynamic ts
          ss' = make_steps_dynamic ss

-- Construct a "dynamic" reduction of length at most omega out of a transfinite
-- reduction whose associated ordinal representation is Omega.
--
-- Note that this function uses the internal structure of the system of notation
-- defined for Omega: the system is assumed to be effectively the identity
-- mapping.
make_omega_trans_dynamic :: (Show s, Show v,
                             Signature s, Variables v, RewriteSystem s v r)
    => T.CReduction s v r Omega
       -> CReduction DynamicSigma DynamicVar DynamicSystem
make_omega_trans_dynamic (T.CRCons (T.RCons ts ss z) phi)
    = CRCons (RCons ts' ss') phi'
    where ts'  = make_terms_dynamic ts
          ss'  = make_steps_dynamic ss
          phi' = \n -> to_int (phi z n)
