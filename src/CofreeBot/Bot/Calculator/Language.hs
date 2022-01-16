{-# OPTIONS -fdefer-typed-holes -Wno-orphans #-}
module CofreeBot.Bot.Calculator.Language where

import CofreeBot.Parser
import Control.Monad.Combinators.NonEmpty qualified as NE
import Text.Megaparsec.Char qualified as MC
import Text.Megaparsec.Char.Lexer qualified as L
import CofreeBot.Utils
import Data.Bifunctor
import Data.Char (isAlpha, isDigit)
import Data.Foldable (Foldable (fold))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Scientific qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Control.Monad.State.Class
import Control.Monad.RWS.Class
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Except
import Data.Coerce

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

space :: Parser ()
space = L.space MC.space1 empty empty

symbol :: Text -> Parser Text
symbol = L.symbol space

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

--------------------------------------------------------------------------------
-- Parsing types
--------------------------------------------------------------------------------

type VarName = T.Text

data Expr
  = Var VarName
  | Val Double
  | Add Expr Expr
  | Sub Expr Expr
  | Mult Expr Expr
  | Div Expr Expr
  | Mod Expr Expr
  | Pow Expr Expr
  | Neg Expr

data Statement
  = Let T.Text Expr
  | StdOut Expr
  deriving Show

type Program = NE.NonEmpty Statement

--------------------------------------------------------------------------------
-- Printer
--------------------------------------------------------------------------------

instance Show Expr where
  showsPrec p = \case
    Var x -> showString $ T.unpack x
    Val n
      | let (i, d) = properFraction n, d == 0 -> shows @Integer i
      | otherwise                             -> shows n
    x `Add` y -> showParen (p >= 6) $ (showsPrec 6 x) . (" + " ++) . (showsPrec 6 y)
    x `Sub` y -> showParen (p >= 6) $ (showsPrec 6 x) . (" - " ++) . (showsPrec 6 y) 
    x `Mult` y -> showParen (p >= 7) $ (showsPrec 7 x) . (" * " ++) . (showsPrec 7 y)
    x `Div` y -> showParen (p >= 7) $ (showsPrec 7 x) . (" / " ++) . (showsPrec 7 y)
    x `Mod` y -> showParen (p >= 7) $ (showsPrec 7 x) . (" % " ++) . (showsPrec 7 y)
    x `Pow` y -> showParen (p >= 8) $ (showsPrec 8 x) . (" ^ " ++) . (showsPrec 8 y)
    Neg x -> shows $ "- " <> show x

--------------------------------------------------------------------------------
-- Parser
--------------------------------------------------------------------------------

varNameP :: Parser VarName
varNameP = lexeme $ T.cons
  <$> letterChar
  <*> takeWhileP Nothing (liftM2 (||) isAlpha isDigit)

valP :: Parser Double
valP = S.toRealFloat <$> lexeme scientific

exprP :: Parser Expr
exprP = makeExprParser (Var <$> varNameP <|> Val <$> valP)
  [ [prefix "-" Neg]
  , [infixR "^" Pow]
  , [infixL "*" Mult, infixL "/" Div, infixL "^" Pow]
  , [infixL "+" Add, infixL "-" Sub]
  ]
 where
  infixL n f = InfixL $ f <$ symbol n
  infixR n f = InfixR $ f <$ symbol n
  prefix n f = Prefix $ f <$ symbol n

statementP :: Parser Statement
statementP = choice
  [ try $ Let <$> varNameP <* symbol ":=" <*> exprP
  , StdOut <$> exprP
  ]

programP :: Parser Program
programP = NE.sepBy1 statementP eol <* eof

-- $> import CofreeBot.Parser

-- $> import CofreeBot.Plugins.Calculator.Language

-- $> parse exprP "((11 + x1) + 13)"

-- $> parse programP "x := ((11 + 12) + 13)\nx + 1"

data ParseError = ParseError
  { parseInput :: T.Text
  , parseError :: T.Text
  }

parseProgram :: T.Text -> Either ParseError Program
parseProgram txt = first (ParseError txt . T.pack) $ parse programP txt

--------------------------------------------------------------------------------
-- Evaluation types
--------------------------------------------------------------------------------

data CalcError
  = LookupError T.Text
  | NotAnInteger Double
  deriving Show

type CalcState = Map.Map T.Text Double

data CalcResp
  = Log Expr Double

--------------------------------------------------------------------------------
-- Evaluator
--------------------------------------------------------------------------------

type CalculatorM = Transformers
  [ WriterT [CalcResp]
  , ExceptT CalcError
  , StateT CalcState
  ] IO

-- | Evaluate an expression in our arithmetic language
eval :: Expr -> CalculatorM Double
eval = \case
  Var bndr -> do
    val <- Map.lookup bndr <$> get
    note (LookupError bndr) val
  Val i -> pure i
  Add x y -> liftM2 (+) (eval x) (eval y)
  Sub x y -> liftM2 (-) (eval x) (eval y)
  Mult x y -> liftM2 (*) (eval x) (eval y)
  Div x y -> liftM2 (/) (eval x) (eval y)
  Mod x y -> fromIntegral <$> liftM2 mod
    (asInteger =<< eval x)
    (asInteger =<< eval y)
  Pow x y -> liftM2 (^) (eval x) (asInteger =<< eval y)
  Neg x -> negate <$> eval x
 where
  asInteger :: Double -> CalculatorM Integer
  asInteger n
    | let (i, d) = properFraction n, d == 0 = pure i
    | otherwise = throwError $ NotAnInteger n

-- | Interpret a language statement into response.
interpretStatement :: Statement -> CalculatorM ()
interpretStatement = \case
  Let bndr expr -> do
    val <- eval expr
    modify $ Map.insert bndr val
  StdOut expr -> do
    val <- eval expr
    tell [Log expr val]

interpretProgram :: Program -> CalcState -> IO (Either CalcError [CalcResp], CalcState)
interpretProgram =
  (fmap . fmap . first . fmap) (snd @()) . coerce . fmap fold . traverse interpretStatement
