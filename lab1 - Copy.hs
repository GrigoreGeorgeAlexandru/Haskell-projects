data Prog = On Instr
data Instr = Off | Expr :> Instr
data Expr = Mem | V Int | Expr :+ Expr
type Env = Int -- valoarea celulei de memorie
type DomProg = [Int]
type DomInstr = Env -> [Int]
type DomExpr = Env -> Int

prog :: Prog -> DomProg
prog = undefined

stmt :: Instr -> DomInstr
stmt = undefined

expr :: Expr -> DomExpr
expr = undefined

p1 = On ( (V 3) :> ((Mem :+ (V 5)):> Off))


type Name = String
data Hask = HTrue
 | HFalse
 | HLit Int
 | HIf Hask Hask Hask
 | Hask :==: Hask
 | Hask :+: Hask
 | HVar Name
 | HLam Name Hask
 | Hask :$: Hask
  deriving (Read, Show)
infix 4 :==:
infixl 6 :+:
infixl 9 :$:

data Value = VBool Bool
 | VInt Int
 | VFun (Value -> Value)
 | VError -- pentru reprezentarea erorilor

instance Show Value where
  show (VBool b) = show b
  show (VInt i) = show i
  show (VFun f) = "functie"
  show (VError) = "eroare"

instance Eq Value where
  (VBool b1) == (VBool b2) = b1 == b2
  (VInt i1) == (VInt i2)  = i1 == i2
  (VFun _) == (VFun _) = error "nu putem egala functii"
  VError == VError = error "nu putem egala erori"
  _ == _ = False

type HEnv = [(Name, Value)]

type DomHask = HEnv -> Value

hEval :: Hask -> DomHask
hEval HTrue env = VBool True
hEval HFalse _ = VBool False
hEval (HLit i) _ = VInt i
hEval (HIf cond c1 c2) env = evalIF (hEval cond env) (hEval c1 env) (hEval c2 env)
   where
     evalIF (VBool b) c d = if b then c else d
     evalIf _ _ _ = VError

hEval (c1 :==: c2) env = evalEq (hEval c1 env) (hEval c2 env)
   where evalEq (VInt i1) (VInt i2) = VBool (i1 == i2)
         evalEq _ _ = VError

hEval (c1 :+: c2) env = evalAdd (hEval c1 env) (hEval c2 env)
  where evalAdd (VInt i1) (VInt i2) = VInt (i1 + i2)
        evalAdd _ _ = VError

hEval (HVar n) env = case lookup n env of
                     Just v -> v
                     Nothing -> VError

hEval (HLam x e) env = VFun ( \ v -> hEval e ( (x,v):env ))

hEval (c1 :$: c2) env = evalApp (hEval c1 env) (hEval c2 env)
   where
     evalApp (VFun f) v = f v
     evalApp _  _ = VError


h = (HLam "x" ((HLam "y" (HVar "x" :+: HVar "y")) :$: (HLit 3))) :$: (HLit 4)
