module TypeInference (TypingJudgment, Result(..), inferType)

where

import Data.List(intersect)
import Exp
import Type
import Unification

------------
-- Errors --
------------
data Result a = OK a | Error String


--------------------
-- Type Inference --
--------------------
type TypingJudgment = (Env, AnnotExp, Type)


inferType :: PlainExp -> Result TypingJudgment
inferType e = case infer' e 0 of
    OK (_, tj) -> OK tj
    Error s -> Error s


infer' :: PlainExp -> Int -> Result (Int, TypingJudgment)

-- COMPLETAR DESDE AQUI

-- infer' (VarExp x)     n = ...


infer' (VarExp s) n =
	OK (n + 1, (extendE emptyEnv s (TVar n), VarExp s, TVar n))

infer' ZeroExp n =
	OK (n, (emptyEnv, ZeroExp, TNat))

infer' (SuccExp u) n =
	case infer' u n of
		Error err -> Error err
		OK (nu, (env, au, t)) -> case mgu [(t, TNat)] of
			UError t1 t2 -> uError t1 t2
			UOK s -> OK (nu, (s <.> env, s <.> SuccExp au, TNat))

infer' (PredExp u) n =
	case infer' u n of
		Error err -> Error err
		OK (nu, (env, au, t)) -> case mgu [(t, TNat)] of
			UError t1 t2 -> uError t1 t2
			UOK s -> OK (nu, (s <.> env, s <.> PredExp au, TNat))

infer' (IsZeroExp u) n =
	case infer' u n of
		Error err -> Error err
		OK (nu, (env, au, t)) -> case mgu [(t, TNat)] of
			UError t1 t2 -> uError t1 t2
			UOK s -> OK (nu, (s <.> env, s <.> IsZeroExp au, TBool))

infer' TrueExp n =
	OK (n, (emptyEnv, TrueExp, TBool))

infer' FalseExp n =
	OK (n, (emptyEnv, FalseExp, TBool))

infer' (IfExp u v w) n =
	case infer' u n of
		Error err -> Error err
		OK (nu, (envu, au, tu)) -> case infer' v nu of
			Error err -> Error err
			OK (nv, (envv, av, tv)) -> case infer' w nv of
				Error err -> Error err
				OK (nw, (envw, aw, tw)) -> case mgu $ [(tv, tw), (tu, TBool)] ++ (unirEnv envu envv) ++ (unirEnv envu envw) ++ (unirEnv envv envw) of
					UError t1 t2 -> uError t1 t2
					UOK s -> OK (nw, (unirSEnv s [envu, envv, envw], s <.> IfExp au av aw, s <.> tv))

infer' (LamExp s x u) n =
	case infer' u n of
		Error err -> Error err
		OK (nu, (envu, au, tu)) -> OK (nu + ntau, (removeE envu s, LamExp s ttau au, TFun ttau tu))
			where ntau = fst tau
			      ttau = snd tau
			      tau = if s `elem` (domainE envu) then (0, evalE envu s) else (1, TVar nu)

infer' (AppExp u v) n =
	case infer' u n of
		Error err -> Error err
		OK (nu, (envu, au, tu)) -> case infer' v nu of
			Error err -> Error err
			OK (nv, (envv, av, tv)) -> case mgu $ [(tu, TFun tv (TVar nv))] ++ unirEnv envu envv of
				UError errt1 errt2 -> uError errt1 errt2
				UOK s -> OK (nv + 1, (unirSEnv s [envu, envv], s <.> AppExp au av, s <.> TVar nv))


unirEnv :: Env -> Env -> [UnifGoal]
unirEnv ex ey = map (\s -> (evalE ex s, evalE ey s)) (intersect (domainE ex) (domainE ey))

unirSEnv :: Subst -> [Env] -> Env
unirSEnv s es = joinE $ map (s <.>) es


--------------------------------
-- YAPA: Error de unificacion --
--------------------------------
uError :: Type -> Type -> Result (Int, a)
uError t1 t2 = Error $ "Cannot unify " ++ show t1 ++ " and " ++ show t2
