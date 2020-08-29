module Lib
  ( main
  )
where

import           Relude
import           Data.Char
import qualified Data.Text                     as T
import           System.Environment
import qualified LuluInclude

newtype Parser a = Parser { parse :: Text -> [(a, Text)] }

runParser :: Parser a -> Text -> Either Text a
runParser m s = case parse m s of
  [(res, "")] -> Right res
  [(_  , cs)] -> Left $ "Parser did not consume entire stream: " <> cs
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
item = Parser $ \s -> case T.uncons s of
  Just (c, cs) -> [(c, cs)]
  Nothing      -> []

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = item >>= \c -> if p c then return c else mzero

oneOf :: Text -> Parser Char
oneOf s = satisfy (flip elem $ toString s)

char :: Char -> Parser Char
char c = satisfy (c ==)

string :: Text -> Parser Text
string s = case T.uncons s of
  Just (c, cs) -> do
    _ <- char c
    _ <- string cs
    return $ T.cons c cs
  Nothing -> return ""

token :: Parser a -> Parser a
token p = do
  a <- p
  _ <- spaces
  return a

reserved :: Text -> Parser Text
reserved = token . string

spaces :: Parser Text
spaces = toText <$> many (oneOf " \n\r")

digit :: Parser Char
digit = satisfy isDigit

alpha :: Parser Char
alpha = satisfy isAlpha

alphanumeric :: Parser Char
alphanumeric = alpha <|> digit

number :: Parser Double
number = do
  sgn <- string "-" <|> return ""
  lhs <- toText <$> some digit
  dot <- string "." <|> return ""
  rhs <- toText <$> some digit <|> return ""
  either (const mzero) return $ readEither $ sgn <> lhs <> dot <> rhs

data LispExpr
  = LispNil
  | LispT
  | LispNumber Double
  | LispName Text
  | LispText Text
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
    cs  <- toText <$> (many $ alphanumeric <|> char '_')
    dot <- liftA2 (T.cons) (char '.') (name) <|> return ""
    return $ (T.cons c cs) <> dot

lispExprText :: Parser LispExpr
lispExprText = LispText <$> (char '"' *> inner <* char '"')
 where
  inner      = toText <$> many (normalChar <|> escapeChar)
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
  lispExprText
    <|> lispExprList
    <|> lispExprName
    <|> lispExprNumber
    <|> lispExprNil
    <|> lispExprT

type LispProgram = [LispExpr]

lispParser :: Parser LispProgram
lispParser = many (token lispExpr) <|> return []

data JSExpr
  = JSText Text
  | JSNumber Double
  | JSBoolean Bool
  | JSNull
  | JSUndefined
  | JSIdentifier Text
  | JSAssignment Text JSExpr
  | JSFunctionDeclaration Text [Text] [JSExpr]
  | JSArrowFunction [Text] [JSExpr]
  | JSCallExpression JSExpr [JSExpr]
  | JSNewExpression Text [JSExpr]
  | JSTernary JSExpr JSExpr JSExpr
  deriving (Show, Eq)

lispToArgList :: [LispExpr] -> Either Text [Text]
lispToArgList args = sequence $ nameToText <$> args
 where
  nameToText (LispName n) = Right n
  nameToText _            = Left "Error converting argument list"

lispToJSExpr :: LispExpr -> Either Text JSExpr
lispToJSExpr expr = case expr of
  LispNil       -> Right JSNull
  LispT         -> Right $ JSBoolean True
  LispNumber n  -> Right $ JSNumber n
  LispName   s  -> Right $ JSIdentifier s
  LispText   s  -> Right $ JSText s
  LispList   [] -> Right $ JSNull
  LispList [LispName "let", LispName n, rest] ->
    liftA2 JSAssignment (return n) (lispToJSExpr rest)
  LispList [LispName "let", _, _] -> Left "Invalid lvalue in `let`"
  LispList (LispName "let" : rest) ->
    Left $ "`let` requires exactly two arguments. Recieved: " <> show rest
  LispList [LispName "if", cond, t, f] ->
    liftA3 JSTernary (lispToJSExpr cond) (lispToJSExpr t) (lispToJSExpr f)
  LispList [LispName "if", cond, t] ->
    liftA3 JSTernary (lispToJSExpr cond) (lispToJSExpr t) (return JSUndefined)
  LispList (LispName "if" : _   ) -> Left "`if` requires one or two arguments"
  LispList (LispName "defun" : LispName name : LispList args : rest) -> do
    argsStr <- lispToArgList args
    body    <- sequence $ lispToJSExpr <$> rest
    return $ JSFunctionDeclaration name argsStr body
  LispList (LispName "defun" : _) -> Left "Function declaration error"
  LispList (LispName "lambda" : LispList args : exprs) -> do
    argsStr <- lispToArgList args
    body    <- sequence $ lispToJSExpr <$> exprs
    return $ JSArrowFunction argsStr body
  LispList (LispName "new" : LispName construct : args) -> liftA2
    JSNewExpression
    (return construct)
    (sequence $ lispToJSExpr <$> args)
  LispList (lvalue : args) -> liftA2 JSCallExpression
                                     (lispToJSExpr lvalue)
                                     (sequence $ lispToJSExpr <$> args)

jsExprsToFnBody :: [JSExpr] -> Text
jsExprsToFnBody []     = ""
jsExprsToFnBody [expr] = "return " <> jsToText expr
jsExprsToFnBody exprs =
  T.intercalate "\n"
    $  ((fmap jsToText) <$> viaNonEmpty init exprs ?: [""])
    <> ["return " <> ((jsToText <$> viaNonEmpty last exprs) ?: "")]

addslashes :: Text -> Text
addslashes str = case T.uncons str of
  Nothing      -> ""
  Just (c, cs) -> case c of
    '"'  -> "\\\"" <> addslashes cs
    '\\' -> "\\\\" <> addslashes cs
    '/'  -> "\\/" <> addslashes cs
    '\b' -> "\\b" <> addslashes cs
    '\f' -> "\\f" <> addslashes cs
    '\n' -> "\\n" <> addslashes cs
    '\r' -> "\\r" <> addslashes cs
    '\t' -> "\\t" <> addslashes cs
    _    -> T.cons c $ addslashes cs

jsToText :: JSExpr -> Text
jsToText expr = case expr of
  JSNull              -> "null"
  JSUndefined         -> "undefined"
  JSBoolean    True   -> "true"
  JSBoolean    False  -> "false"
  JSNumber     n      -> show n
  JSIdentifier s      -> s
  JSText       s      -> "\"" <> addslashes s <> "\""
  JSAssignment s rest -> s <> " = " <> jsToText rest
  JSTernary cond t f ->
    jsToText cond <> " ? " <> jsToText t <> " : " <> jsToText f
  JSArrowFunction args body ->
    "(" <> T.intercalate "," args <> ") => {\n" <> jsExprsToFnBody body <> "\n}"
  JSCallExpression lhs args ->
    jsToText lhs <> "(" <> (T.intercalate "," $ jsToText <$> args) <> ")"
  JSNewExpression construct args ->
    "new " <> construct <> "(" <> (T.intercalate "," $ jsToText <$> args) <> ")"
  JSFunctionDeclaration name args body ->
    "function "
      <> name
      <> "("
      <> T.intercalate "," args
      <> ") {\n"
      <> jsExprsToFnBody body
      <> "\n}"

compiler :: Text -> Either Text Text
compiler input = do
  lispProgram <- runParser lispParser input
  jsProgram   <- sequence $ lispToJSExpr <$> lispProgram
  return $ T.intercalate "\n" $ jsToText <$> jsProgram

runCompiler :: FilePath -> FilePath -> IO ()
runCompiler fin fout = do
  contents <- compiler <$> readFileText fin
  either (printAndFail)
         (writeFileText fout)
         ((LuluInclude.includeHeader <>) <$> contents)
 where
  printAndFail str = do
    putTextLn str
    exitFailure

main :: IO ()
main = do
  getArgs >>= matchArgs
 where
  matchArgs [fin, fout] = runCompiler fin fout
  matchArgs _           = usage
  usage = putStrLn "Usage: <input file> <output file>"
