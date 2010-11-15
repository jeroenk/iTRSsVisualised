module DynamicOmegaReductions (
    DynamicSigma(DynamicFun),
    DynamicVar(DynamicVar),
    DynamicSystem,
    make_omega_dynamic,
    make_omega_trans_dynamic
) where

import MyShow
import SignatureAndVariables
import Terms
import RulesAndSystems
import qualified OmegaReductions as O
import SystemsOfNotation
import qualified TransfiniteReductions as T

import Array

-- A "dynamic" signature: the arity is encoded in the function symbol, where
-- the actual function symbol is represented by a string.
data DynamicSigma = DynamicFun String Int

instance MyShow DynamicSigma where
    myshow (DynamicFun f _) = f

instance Eq DynamicSigma where
    (DynamicFun f a) == (DynamicFun g b) = (f == g) && (a == b)

instance Signature DynamicSigma where
    arity (DynamicFun _ a) = a

-- A "dynamic" variable set: variables are represented by strings.
data DynamicVar = DynamicVar String

instance MyShow DynamicVar where
    myshow (DynamicVar x) = x

instance Eq DynamicVar where
    (DynamicVar x) == (DynamicVar y) = x == y

instance Variables DynamicVar

-- A "dynamic" rewrite system: the rules are simply not know. This still makes
-- sense in the context of the confluence and compression algorithms, as those
-- use the rules embedded in the reductions they are applied to.
data DynamicSystem = DynamicSystem

instance RewriteSystem DynamicSigma DynamicVar DynamicSystem where
    rules DynamicSystem = error "Rules cannot be queried in dynamic systems"


make_term_dynamic :: (MyShow s, MyShow v, Signature s, Variables v)
    => Term s v -> Term DynamicSigma DynamicVar
make_term_dynamic (Function f ss) = Function f' ss'
    where f'  = DynamicFun (myshow f) a
          ss' = array (1, a) [(i, make_term_dynamic (ss!i)) | i <- indices ss]
          a   = arity f
make_term_dynamic (Variable x)    = Variable x'
    where x' = DynamicVar (myshow x)

make_terms_dynamic :: (MyShow s, MyShow v, Signature s, Variables v)
    => [Term s v] -> [Term DynamicSigma DynamicVar]
make_terms_dynamic ts = map make_term_dynamic ts

make_step_dynamic :: (MyShow s, MyShow v, Signature s, Variables v)
    => Step s v -> Step DynamicSigma DynamicVar
make_step_dynamic (ps, Rule l r) = (ps, Rule l' r')
    where l' = make_term_dynamic l
          r' = make_term_dynamic r

make_steps_dynamic :: (MyShow s, MyShow v, Signature s, Variables v)
    => [Step s v] -> [Step DynamicSigma DynamicVar]
make_steps_dynamic ss = map make_step_dynamic ss

make_omega_dynamic :: (MyShow s, MyShow v,
                       Signature s, Variables v, RewriteSystem s v r)
    => O.CReduction s v r -> O.CReduction DynamicSigma DynamicVar DynamicSystem
make_omega_dynamic (O.CRConst (O.RConst ts ss) phi)
    = O.CRConst (O.RConst ts' ss') phi
    where ts' = make_terms_dynamic ts
          ss' = make_steps_dynamic ss

-- Note that this function uses the internal structure of the system of notation
-- defined for Omega: the system is assumed to be effectively the identity
-- mapping.
make_omega_trans_dynamic :: (MyShow s, MyShow v,
                             Signature s, Variables v, RewriteSystem s v r)
    => T.CReduction s v r Omega
       -> O.CReduction DynamicSigma DynamicVar DynamicSystem
make_omega_trans_dynamic (T.CRConst (T.RConst ts ss z) phi)
    = O.CRConst (O.RConst ts' ss') phi'
    where ts'  = make_terms_dynamic ts
          ss'  = make_steps_dynamic ss
          phi' = \n -> to_int (phi z n)
