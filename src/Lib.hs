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
  [(_  , _ )] -> Left "Parser did not consume entire stream"
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
lispExprNil = LispNil <$ (string "nil" <|> string "()")

lispExprT :: Parser LispExpr
lispExprT = LispT <$ string "t"

lispExprName :: Parser LispExpr
lispExprName = LispName <$> name
 where
  name = do
    c   <- alpha
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
  lispExprNil
    <|> lispExprT
    <|> lispExprNumber
    <|> lispExprString
    <|> lispExprName
    <|> lispExprList

type LispProgram = [LispExpr]

lispParser :: Parser LispProgram
lispParser = many (token lispExpr) <|> return []

data JSExpr
  = JSString String
  | JSNumber Double
  | JSBoolean Bool
  | JSNull
  | JSIdentifier String
  | JSAssignment String JSExpr
  | JSFunctionDeclaration String [String] [JSExpr]
  | JSCallExpression JSExpr [JSExpr]
  deriving (Show, Eq)

lispToJSExpr :: LispExpr -> JSExpr
lispToJSExpr LispNil         = JSNull
lispToJSExpr LispT           = JSBoolean True
lispToJSExpr (LispNumber n ) = JSNumber n
lispToJSExpr (LispName   s ) = JSIdentifier s
lispToJSExpr (LispString s ) = JSString s
lispToJSExpr (LispList   []) = JSNull
lispToJSExpr (LispList (name : args)) =
  case name of
    (LispName "let") -> undefined
    (LispName "defun") -> undefined
    _ -> JSCallExpression (lispToJSExpr name) (lispToJSExpr <$> args)

jsToString :: JSExpr -> String
jsToString JSNull               = "null"
jsToString (JSBoolean    True ) = "true"
jsToString (JSBoolean    False) = "false"
jsToString (JSNumber     n    ) = show n
jsToString (JSIdentifier s    ) = s
jsToString (JSString     s    ) = '"' : s ++ "\""
jsToString (JSAssignment s expr) = undefined
jsToString (JSFunctionDeclaration s args exprs) = undefined
jsToString (JSCallExpression expr args) =
  jsToString expr ++ "(" ++ (intercalate "," $ jsToString <$> args) ++ ")"

compiler :: String -> Either String String
compiler input = do
  lisp <- runParser lispParser input
  return $ intercalate "\n" $ jsToString <$> lispToJSExpr <$> lisp

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
