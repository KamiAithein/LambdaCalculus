module Model
    (   Param(..),
        Body(..),
        Variable(..),
        Function(..),
        Expr(..),
        simplify,
        applyf
    ) where

import Data.Function

newtype Variable rep = Variable rep deriving Eq

instance (Show rep) => Show (Variable rep) where
    show (Variable v) = show v

type Param rep = Variable rep
type Body rep = Expr rep
data Function rep = Function (Param rep) (Body rep) deriving Eq

instance (Show rep) => Show (Function rep) where
    show (Function p b) = "(\\" ++ show p ++ "." ++ show b ++ ")"

data Expr rep = Exprv (Variable rep) 
              | Exprf (Function rep) 
              | Expre (Expr rep) (Expr rep) deriving Eq

instance (Show rep) => Show (Expr rep) where
    show (Exprv v) = "(" ++ show v ++ ")"
    show (Exprf f) = "(" ++ show f ++ ")"
    show (Expre el er) = "(" ++ show el ++ show er ++ ")"

data Repr base = Repr 
    {
        baseOf :: base,
        reprOf :: Integer
    }

instance Eq (Repr rep) where
    rl == rr = reprOf rl == reprOf rr

applyf :: (Eq rep) => Function (Repr rep) -> Expr (Repr rep) -> Expr (Repr rep)
applyf (Function p b) a = subWith p a b
    where subWith :: (Eq rep) => Variable (Repr rep) -> Expr (Repr rep) -> Expr (Repr rep) -> Expr (Repr rep)
          -- (p.p)a = a
          -- (p.b)a = b
          subWith p a body@(Exprv b) | p == b    = a
                                     | otherwise = body
          -- (p.(p'.p))a = (p'.a)
          -- (p.(p'.b))a = (p'.b)
          -- or,
          -- (p.(p'.B))a = (p'. subWith p a B)
          subWith p a body@(Exprf (Function p' b)) = Exprf $ Function p' $ subWith p a b

          -- (p.LR)a = simplify (subWith p a L)(subWith p a R)
          subWith p a body@(Expre el er) = simplify $ Expre (subWith p a el) (subWith p a er)

simplify :: (Eq rep) => Expr rep -> Expr rep
-- v = v
simplify (Exprv v) = Exprv v
-- (p.B) = (p. simplify B)
simplify (Exprf (Function p b)) = Exprf $ Function p $ simplify b
-- v _ = v _
simplify (Expre v@(Exprv _) er) = Expre v $ simplify er
-- (p.B) a = applyf (p.B) a
simplify (Expre (Exprf f) a) = simplify $ applyf f a
-- LMR = simplify (simplify L) (simplify MR)
simplify (Expre (Expre el er) v)  = simplify $ Expre (simplify el) $ simplify $ Expre er v