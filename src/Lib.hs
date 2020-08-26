module Lib
  ( main
  )
where

import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Data.List
import           System.Environment
import           System.Exit
import qualified LuluInclude

newtype Parser a = Parser { parse :: String -> [(a, String)] }

runParser :: Parser a -> String -> Either String a
runParser m s = case parse m s of
  [(res, [])] -> Right res
  [(_  , cs)] -> Left $ "Parser did not consume entire stream: " ++ cs
  _           -> Left "Parse error"

instance Functor Parser where
  fmap f (Parser cs) = Parser $ \s -> [ (f a, b) | (a, b) <- cs s ]

instance Applicative Parser where
  pure = return
  (Parser cs1) <*> (Parser cs2) =
    Parser $ \s -> [ (f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1 ]

instance Monad Parser where
  return a = Parser $ \s -> [(a, s)]
  p >>= f = Parser $ \s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s

instance MonadPlus Parser where
  mzero = Parser $ const []
  mplus p q = Parser $ \s -> parse p s ++ parse q s

instance Alternative Parser where
  empty = mzero
  p <|> q = Parser $ \s -> case parse p s of
    []  -> parse q s
    res -> res

item :: Parser Char
item = Parser $ \s -> case s of
  []       -> []
  (c : cs) -> [(c, cs)]

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = item >>= \c -> if p c then return c else mzero

oneOf :: [Char] -> Parser Char
oneOf s = satisfy (flip elem s)

char :: Char -> Parser Char
char c = satisfy (c ==)

string :: String -> Parser String
string []       = return []
string (c : cs) = do
  _ <- char c
  _ <- string cs
  return (c : cs)

token :: Parser a -> Parser a
token p = do
  a <- p
  _ <- spaces
  return a

reserved :: String -> Parser String
reserved = token . string

spaces :: Parser String
spaces = many $ oneOf " \n\r"

digit :: Parser Char
digit = satisfy isDigit

alpha :: Parser Char
alpha = satisfy isAlpha

alphanumeric :: Parser Char
alphanumeric = alpha <|> digit

number :: Parser Double
number = do
  sgn <- string "-" <|> return []
  lhs <- some digit
  dot <- string "." <|> return []
  rhs <- some digit <|> return []
  return $ read $ sgn ++ lhs ++ dot ++ rhs

data LispExpr
  = LispNil
  | LispT
  | LispNumber Double
  | LispName String
  | LispString String
  | LispList [LispExpr]
  deriving (Show, Eq)

lispExprNumber :: Parser LispExpr
lispExprNumber = LispNumber <$> number

lispExprNil :: Parser LispExpr
lispExprNil = LispNil <$ string "nil"

lispExprT :: Parser LispExpr
lispExprT = LispT <$ string "t"

lispExprName :: Parser LispExpr
lispExprName = LispName <$> name
 where
  name = do
    c   <- alpha <|> char '_'
    cs  <- many $ alphanumeric <|> char '_'
    dot <- liftA2 (:) (char '.') (name) <|> return []
    return $ (c : cs) ++ dot

lispExprString :: Parser LispExpr
lispExprString =
  LispString <$> (char '"' *> many (normalChar <|> escapeChar) <* char '"')
 where
  normalChar = satisfy $ liftA2 (&&) ('"' /=) ('\\' /=)
  escapeChar =
    ('"' <$ string "\\\"")
      <|> ('\\' <$ string "\\\\")
      <|> ('/' <$ string "\\/")
      <|> ('\b' <$ string "\\b")
      <|> ('\f' <$ string "\\f")
      <|> ('\n' <$ string "\\n")
      <|> ('\r' <$ string "\\r")
      <|> ('\t' <$ string "\\t")

lispExprList :: Parser LispExpr
lispExprList = do
  _ <- reserved "("
  a <- many $ token $ lispExpr
  _ <- reserved ")"
  return $ LispList a

lispExpr :: Parser LispExpr
lispExpr =
  lispExprString
    <|> lispExprList
    <|> lispExprName
    <|> lispExprNumber
    <|> lispExprNil
    <|> lispExprT

type LispProgram = [LispExpr]

lispParser :: Parser LispProgram
lispParser = many (token lispExpr) <|> return []

data JSExpr
  = JSString String
  | JSNumber Double
  | JSBoolean Bool
  | JSNull
  | JSUndefined
  | JSIdentifier String
  | JSAssignment String JSExpr
  | JSFunctionDeclaration String [String] [JSExpr]
  | JSArrowFunction [String] [JSExpr]
  | JSCallExpression JSExpr [JSExpr]
  | JSNewExpression String [JSExpr]
  | JSTernary JSExpr JSExpr JSExpr
  deriving (Show, Eq)

lispToArgList :: [LispExpr] -> Either String [String]
lispToArgList args = sequence $ nameToString <$> args
 where
  nameToString (LispName n) = Right n
  nameToString _            = Left "Error converting argument list"

lispToJSExpr :: LispExpr -> Either String JSExpr
lispToJSExpr LispNil         = Right JSNull
lispToJSExpr LispT           = Right $ JSBoolean True
lispToJSExpr (LispNumber n ) = Right $ JSNumber n
lispToJSExpr (LispName   s ) = Right $ JSIdentifier s
lispToJSExpr (LispString s ) = Right $ JSString s
lispToJSExpr (LispList   []) = Right JSNull
lispToJSExpr (LispList (LispName "let" : LispName n : expr : [])) =
  liftA2 JSAssignment (return n) (lispToJSExpr expr)
lispToJSExpr (LispList (LispName "let" : _ : _ : [])) =
  Left "Invalid lvalue in `let`"
lispToJSExpr (LispList (LispName "let" : rvalue)) =
  Left $ "`let` requires exactly two arguments. Recieved: " ++ show rvalue
lispToJSExpr (LispList (LispName "if" : cond : t : f : [])) =
  liftA3 JSTernary (lispToJSExpr cond) (lispToJSExpr t) (lispToJSExpr f)
lispToJSExpr (LispList (LispName "if" : cond : t : [])) =
  liftA3 JSTernary (lispToJSExpr cond) (lispToJSExpr t) (return JSUndefined)
lispToJSExpr (LispList (LispName "if" : _)) = Left "Ternary condition error"
lispToJSExpr (LispList (LispName "defun" : LispName name : LispList args : exprs))
  = do
    argsStr <- lispToArgList args
    body    <- sequence $ lispToJSExpr <$> exprs
    return $ JSFunctionDeclaration name argsStr body
lispToJSExpr (LispList (LispName "defun" : _)) =
  Left "Function declaration error"
lispToJSExpr (LispList (LispName "lambda" : LispList args : exprs)) = do
  argsStr <- lispToArgList args
  body    <- sequence $ lispToJSExpr <$> exprs
  return $ JSArrowFunction argsStr body
lispToJSExpr (LispList (LispName "new" : LispName construct : args)) =
  liftA2 JSNewExpression (return construct) (sequence $ lispToJSExpr <$> args)
lispToJSExpr (LispList (lvalue : args)) = liftA2
  JSCallExpression
  (lispToJSExpr lvalue)
  (sequence $ lispToJSExpr <$> args)

jsExprsToFnBody :: [JSExpr] -> String
jsExprsToFnBody []     = ""
jsExprsToFnBody [expr] = "return " ++ jsToString expr
jsExprsToFnBody exprs =
  intercalate "\n"
    $  (jsToString <$> init exprs)
    ++ ["return " ++ jsToString (last exprs)]

jsToString :: JSExpr -> String
jsToString JSNull                = "null"
jsToString JSUndefined           = "undefined"
jsToString (JSBoolean    True  ) = "true"
jsToString (JSBoolean    False ) = "false"
jsToString (JSNumber     n     ) = show n
jsToString (JSIdentifier s     ) = s
jsToString (JSAssignment s expr) = s ++ "=" ++ jsToString expr
jsToString (JSTernary cond t f) =
  jsToString cond ++ " ? " ++ jsToString t ++ " : " ++ jsToString f
jsToString (JSArrowFunction args body) =
  "(" ++ intercalate "," args ++ ") => {\n" ++ jsExprsToFnBody body ++ "\n}"
jsToString (JSCallExpression expr args) =
  jsToString expr ++ "(" ++ (intercalate "," $ jsToString <$> args) ++ ")"
jsToString (JSNewExpression construct args) =
  "new " ++ construct ++ "(" ++ (intercalate "," $ jsToString <$> args) ++ ")"
jsToString (JSString s) = '"' : addslashes s ++ "\""
 where
  addslashes ('"'  : cs) = "\\\"" ++ addslashes cs
  addslashes ('\\' : cs) = "\\\\" ++ addslashes cs
  addslashes ('/'  : cs) = "\\/" ++ addslashes cs
  addslashes ('\b' : cs) = "\\b" ++ addslashes cs
  addslashes ('\f' : cs) = "\\f" ++ addslashes cs
  addslashes ('\n' : cs) = "\\n" ++ addslashes cs
  addslashes ('\r' : cs) = "\\r" ++ addslashes cs
  addslashes ('\t' : cs) = "\\t" ++ addslashes cs
  addslashes (c    : cs) = c : addslashes cs
  addslashes []          = []
jsToString (JSFunctionDeclaration name args body) =
  "function "
    ++ name
    ++ "("
    ++ intercalate "," args
    ++ ") {\n"
    ++ jsExprsToFnBody body
    ++ "\n}"

compiler :: String -> Either String String
compiler input = do
  lispProgram <- runParser lispParser input
  jsProgram   <- sequence $ lispToJSExpr <$> lispProgram
  return $ intercalate "\n" $ jsToString <$> jsProgram

runCompiler :: String -> String -> IO ()
runCompiler fin fout = do
  contents <- compiler <$> readFile fin
  either (printAndFail)
         (writeFile fout)
         ((LuluInclude.includeHeader ++) <$> contents)
 where
  printAndFail str = do
    putStrLn str
    exitWith (ExitFailure 1)

main :: IO ()
main = do
  getArgs >>= matchArgs
 where
  matchArgs [fin, fout] = runCompiler fin fout
  matchArgs _           = usage
  usage = putStrLn "Usage: <input file> <output file>"
