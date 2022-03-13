module LetLang where

import Text.Megaparsec ( Parsec, many, (<?>), between, (<|>), parseTest, parse )
import Data.Void ( Void )
import Text.Megaparsec.Char ( space1, letterChar, alphaNumChar, string, char ) 
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

{-
prog :  expr
expr: = "-" "(" expr "," expr")"
expr: = "zero?" "(" expr ")"
expr: = "if" expr "then" expr "else" expr
expr: "let" NAME "=" expr "in" expr
expr: NAME | NUM
-}

data Value = VInt Int | VBool Bool
    deriving (Show, Eq)

data Expr = ELet String Expr Expr
          | EIf Expr Expr Expr
          | EZero Expr
          | ESub Expr Expr
          | ENum Int
          | EVar String
          deriving (Show, Eq)

type Env = [(String, Value)]

sc :: Parser ()
sc = L.space space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

pNum :: Parser Expr
pNum = ENum <$> lexeme L.decimal

pVar :: Parser Expr
pVar = EVar <$> lexeme ((:) <$> letterChar <*> many alphaNumChar <?> "variable")

parens = between (lexeme $ char '(') (lexeme $ char ')')

pZero :: Parser Expr
pZero = EZero <$> (lexeme (string "zero?") *> parens pExpr)

pIf :: Parser Expr
pIf = EIf <$> (lexeme (string "if") *> pExpr)
            <*> (lexeme (string "then") *> pExpr)
            <*> (lexeme (string "else") *> pExpr)

pLet :: Parser Expr
pLet = ELet <$> (lexeme (string "let") *> lexeme ((:) <$> letterChar <*> many alphaNumChar <?> "variable"))
            <*> (lexeme (string "=") *> pExpr)
            <*> (lexeme (string "in") *> pExpr)
pSub :: Parser Expr
pSub = ESub <$> (lexeme (string "-") *> lexeme (char '(') *> pExpr)
            <*> (lexeme (char ',') *> pExpr <* lexeme (char ')'))

pExpr :: Parser Expr
pExpr = pZero <|> pIf <|> pLet <|> pSub <|> pNum <|> pVar

eval :: Expr -> Env -> Value
eval (ENum n) _ = VInt n
eval (EZero e) env = VBool (eval e env == VInt 0)
eval (ESub e1 e2) env = VInt (v1 - v2)
    where
        (VInt v1) = eval e1 env
        (VInt v2) = eval e2 env
eval (EIf e1 e2 e3) env = if eval e1 env == VBool True then eval e2 env else eval e3 env
eval (EVar s) env = case lookup s env of
    Just v -> v
    Nothing -> error ("variable " ++ s ++ " not found")
eval (ELet var e1 e2) env = eval e2 ((var, eval e1 env) : env)

testLetLang :: IO ()
testLetLang = do
    let env = []
    case parse pExpr "" "let x = 1 in zero?(x)" of
        Left _ -> error "parse error"
        Right a -> do
            print a
            print $ eval a env
