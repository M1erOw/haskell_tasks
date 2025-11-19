import Data.Map (Map)
import qualified Data.Map as Map
import Text.Printf (printf)

stdUnOps :: Map String (Double -> Double)
stdUnOps = Map.fromList
  [ ("id",id),
    ("sin", sin),
    ("cos", cos),
    ("sqrt", sqrt),
    ("negate", negate),
    ("log",log)
  ]


stdBinOps :: Map String (Double -> Double -> Double)
stdBinOps = Map.fromList
  [ ("+", (+)),
    ("-", (-)),
    ("*", (*)),
    ("/", (/)),
    ("max", max),
    ("min", min)
  ]

data Expr
  = Const Double
  | Var String
  | UnOp String Expr
  | BinOp String Expr Expr
  deriving (Eq)

expr = BinOp "+" -- x * 5 + sin(pi / 2)
            (BinOp "*"
                (Var "x")
                (Const 5)
            )
            (UnOp "sin"
                (BinOp "/"
                    (Const pi)
                    (Const 2)
                )
            ) 

showExpr :: Expr -> String
showExpr (Const x) = printf "%.2g" x
showExpr (Var name) = name
showExpr (UnOp op e) = op ++ "(" ++ showExpr e ++ ")"
showExpr (BinOp op e1 e2) = "(" ++ showExpr e1 ++ " " ++ op ++ " " ++ showExpr e2 ++ ")"

-- ghci> showExpr expr
-- "((x * 5.00) + sin((3.14 / 2.00)))"

varMap :: Map String Double
varMap = Map.fromList
  [ ("x", 3.0),
    ("y", 5.0)
  ]

xId x y = x -- Для поиска BinOp

calculate :: Expr -> Double
calculate (Const x) = x
calculate (Var name) = Map.findWithDefault 0 name varMap -- Можно было бы использовать Map.lookup, но тогда ругается на то,
                                                         -- что результат Maybe Double, a не Double
calculate (UnOp op e) = Map.findWithDefault id op stdUnOps $ calculate e
calculate (BinOp op e1 e2) = f x1 x2 where
  f = Map.findWithDefault xId op stdBinOps
  x1 = calculate e1
  x2 = calculate e2

-- ghci> calculate expr
-- 16.0

simplify :: Expr -> Expr
simplify (UnOp op e) = simplifyUn op (simplify e) where
  simplifyUn :: String -> Expr -> Expr
  simplifyUn op (Const x) = Const $ Map.findWithDefault id op stdUnOps x
  simplifyUn op exp = UnOp op exp
simplify (BinOp op e1 e2) = simplifyBin op (simplify e1) (simplify e2) where
  simplifyBin :: String -> Expr -> Expr -> Expr
  -- simplifyBin op (Const x) (Const y) = Const $ Map.findWithDefault xId op stdBinOps x y
  -- simplifyBin "+" e (Const 0) = e
  -- simplifyBin "+" (Const 0) e = e
  -- simplifyBin "-" e (Const 0) = e
  -- simplifyBin "-" (Const 0) e = UnOp "negate" e
  -- simplifyBin "*" e (Const 0) = Const 0
  -- simplifyBin "*" (Const 0) e = Const 0
  -- simplifyBin "*" e (Const 1) = e
  -- simplifyBin "*" (Const 1) e = e
  simplifyBin op (Const x) (Const y) = Const $ Map.findWithDefault xId op stdBinOps x y
  simplifyBin op e1 e2
    |op == "+" && isZero e1 = e2
    |op == "+" && isZero e2 = e1
    |op == "-" && isZero e2 = e1
    |op == "-" && isZero e1 = simplify $ UnOp "negate" e2
    |op == "*" && (isZero e1 || isZero e2)  = Const 0
    |op == "*" && isOne e1 = e1
    |op == "*" && isOne e2 = e2
    |op == "-" && e1 == e2 = Const 0
    |otherwise = BinOp op e1 e2
    where
      isZero (Const 0) = True
      isZero _ = False
      isOne (Const 1) = True
      isOne _ = False
simplify exp = exp -- Переменные и константы

-- ghci> showExpr expr
-- "((x * 5.00) + sin((3.14 / 2.00)))"
-- ghci> showExpr $ simplify expr
-- "((x * 5.00) + 1.00)"