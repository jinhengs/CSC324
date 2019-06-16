module A4 where

import           Control.Applicative
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           A4Def
import           ParserLib

-- This can help testing by reading from a file so you can test multi-line input
-- and also have little hassle with \
parseFile :: String -> IO (Maybe Expr)
parseFile filename = do
    inp <- readFile filename
    let ans = runParser mainParser inp
    return ans

mainParser :: Parser Expr
mainParser = whitespaces *> block <* eof
  where
    block = cond <|> lambda <|> let_ <|> infix_
    infix_ = do
        arth1 <- arith
        (do cp <- cmp
            arth2 <- arith
            pure (Prim2 cp arth1 arth2)
            ) 
            <|> pure arth1
    cmp = (operator "==") *> pure Eq
        <|> (operator "<") *> pure Lt
    arith = chainl1 addend addop
    addop = fmap Prim2 (
        operator "+" *> pure Plus
        <|> operator "-" *> pure Minus
        )
    addend = chainl1 factor mulop
    mulop = fmap Prim2 (
        operator "*" *> pure Mul
        <|> operator "/" *> pure Div
        <|> operator "%" *> pure Mod
        )
    factor = fmap (foldl1 App) (some atom)
    atom = between (char '(' *> whitespaces)
                   (char ')' *> whitespaces)
                   block
       <|> literal
       <|> fmap Var var
    cond = do
        keyword "if"
        _if <- block
        keyword "then"
        _then <- block
        keyword "else"
        _else <- block
        pure (Cond _if _then _else)
    lambda = do
        operator "\\"
        v <- var
        operator "->"
        b <- block
        pure (Lambda v b)
    let_ = do
        keyword "let"
        eqns <- many equation
        keyword "in"
        b <- block
        pure (Let eqns b)
    equation = do
        v <- var
        char '=' *> whitespaces
        b <- block
        char ';' *> whitespaces
        pure (v, b)
    literal = fmap Num integer <|> boolean
    boolean = do
        a <- (keyword "True") <|> (keyword "False")
        return $ case a of
                "True"  -> Bln True
                "False" -> Bln False
    var = identifier ["if", "then", "else", "let", "in", "True", "False"]

-- =============================================================================

mainInterp :: Expr -> Either Error Value
mainInterp = interp Map.empty

intOrDie :: Value -> Either Error Integer
intOrDie (VN i) = pure i
intOrDie _ = Left TypeError

trueDiv :: Integer -> Integer -> Either Error Value
trueDiv numerator denominator = 
    case denominator of 
        0 -> Left DivByZero
        _ -> pure (VN (numerator `div` denominator))

trueMod :: Integer -> Integer -> Either Error Value
trueMod i j = -- i mod j
    case j of 
        0 -> Left DivByZero
        _ -> pure (VN (i `mod` j))

interp :: Map String Value -> Expr -> Either Error Value
interp _ (Num i) = pure (VN i)
interp _ (Bln b) = pure (VB b)
interp env (Var v) = case Map.lookup v env of
    Just a -> pure a
    Nothing -> Left VarNotFound
-- (+)
interp env (Prim2 Plus e1 e2) = do
    a <- interp env e1
    i <- intOrDie a
    b <- interp env e2
    j <- intOrDie b
    pure (VN (i + j))
-- (-)
interp env (Prim2 Minus e1 e2) = do
    a <- interp env e1
    i <- intOrDie a
    b <- interp env e2
    j <- intOrDie b
    pure (VN (i - j))
-- (*)
interp env (Prim2 Mul e1 e2) = do
    a <- interp env e1
    i <- intOrDie a
    b <- interp env e2
    j <- intOrDie b
    pure (VN (i * j))
-- (//)
interp env (Prim2 Div e1 e2) = do
    a <- interp env e1
    i <- intOrDie a
    b <- interp env e2
    j <- intOrDie b
    trueDiv i j
-- (==)
interp env (Prim2 Eq e1 e2) = do
    a <- interp env e1
    i <- intOrDie a
    b <- interp env e2
    j <- intOrDie b
    pure (VB (i == j))
-- (<)
interp env (Prim2 Lt e1 e2) = do
    a <- interp env e1
    i <- intOrDie a
    b <- interp env e2
    j <- intOrDie b
    pure (VB (i < j))
-- (%)
interp env (Prim2 Mod e1 e2) = do
    a <- interp env e1
    i <- intOrDie a
    b <- interp env e2
    j <- intOrDie b
    trueMod i j
-- (Cond)
interp env (Cond test eThen eElse) = do
    a <- interp env test
    case a of
        (VB True) -> interp env eThen
        (VB False) -> interp env eElse
        _ -> Left TypeError
-- (Let)
interp env (Let eqns evalMe) = do
    env' <- extend env eqns
    interp env' evalMe
  where
    extend env [] = pure env
    extend env ((v,rhs) : eqns) = do
        a <- interp env rhs
        let env' = Map.insert v a env
        extend env' eqns
-- (Lambda)
interp env (Lambda v body) = pure (VClosure env v body)
-- (App)
interp env (App f e) = do
    c <- interp env f
    case c of
      VClosure fEnv v body -> do
          eVal <- interp env e
          let bEnv = Map.insert v eVal fEnv
          interp bEnv body
      _ -> Left TypeError
