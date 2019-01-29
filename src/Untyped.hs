{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
module Untyped where

import           Control.Monad                  ( void )
import           Control.Monad.IO.Class
import           Control.Monad.State.Strict     ( MonadState(..)
                                                , modify'
                                                , StateT(..)
                                                )
import           Text.Megaparsec.Char           ( space1
                                                , letterChar
                                                , space
                                                )
import           Text.Megaparsec
import           Text.Megaparsec.Debug          ( dbg )
import           Data.Void
import           Control.Applicative            ( (<*) )
import           System.IO                      ( hFlush
                                                , stdout
                                                )

type Name = String

-- Our untyped lambda terms:
data UntypedLC = UVar Name | -- Atomic variables
                 ULam Name UntypedLC | -- λx.t
                 UApp UntypedLC UntypedLC -- t t
                 deriving (Eq, Ord)

-- Extremely lazy implementation
instance Show UntypedLC where
    show (UVar x) = x
    show (ULam n e) = "λ" ++ n ++ ". " ++ show e
    show (UApp n e) = "(" ++ show n ++ ") (" ++ show e ++ ")"

uvar :: String -> UntypedLC
uvar s = UVar s

-- Perform replacement for a particular name-bound term.
replace :: UntypedLC -> Name -> UntypedLC -> UntypedLC
replace er n (UApp x y) = UApp (replace er n x) (replace er n y)
replace er n (UVar s  ) = if s == n then er else UVar s
replace er n (ULam n2 e) =
  if n == n2 then (ULam n2 e) else ULam n2 (replace er n e)

-- Evaluate our lambda calculus terms using the call by name strategy
-- By this, we reduce the outermost redexes in an expression, with a few quips:
--   1. Abstractions cannot be reduced any further
--   2. Terms in applications are _not_ evaluated before capture-avoiding substitution. Thus, we only
--      evaluate expressions which are used in an evaluation step.
eval
  :: (Show a, Num a, MonadState a m, MonadIO m)
  => (UntypedLC -> m ()) -- Printing function for top-level steps.
  -> UntypedLC -- Expression to evaluate
  -> m UntypedLC -- Evaluated expression
eval f (UApp l r) = do
  l' <- eval nop l
  if l' /= l then f (UApp l' l) else pure ()
  apply l' r
 where
  nop _ = pure ()
  apply (ULam name lamexpr) app =
    let e' = replace app name lamexpr in f e' *> eval f e'
  apply l r = pure (UApp l r)
eval _ a = pure a

-- Run our evaluator, print each reduction as well as the original term.
runEval :: UntypedLC -> IO ()
runEval a = do
  putStrLn $ "Original Term: " <> show a
  (e, s) <- runStateT (eval p a) 0
  putStrLn $ "Final result after " <> show s <> " reductions: " <> show e
 where
  p e = do
    modify' (+ 1)
    ctr <- get
    liftIO $ putStrLn $ show ctr <> ". " <> show e

-- S combinator
su :: UntypedLC
su = ULam
  "x"
  (ULam
    "y"
    (ULam "z" (UApp (UApp (uvar "x") (uvar "z")) (UApp (uvar "y") (uvar "z"))))
  )

-- K combinator
ku :: UntypedLC
ku = ULam "x" (ULam "y" (uvar "x"))

-- I combinator
iu :: UntypedLC
iu = ULam "x" (uvar "x")

-- Church boolean true value
truu :: UntypedLC
truu = ULam "x" (ULam "y" (UVar "x"))

-- Church boolean false
flsu :: UntypedLC
flsu = ULam "x" (ULam "y" (UVar "y"))

-- Chuch logical and
andu :: UntypedLC
andu = ULam "p" (ULam "q" (UApp (UApp (uvar "p") (uvar "q")) (UVar "p")))

type Parser a = Parsec Void String a

parseULC :: Parser UntypedLC
parseULC = do
  l <- some $ (try parseNested <|> try parseVar <|> try parseLam) <* space
  pure $ foldl1 UApp l

parseIdent :: Parser String
parseIdent = some letterChar

parseVar :: Parser UntypedLC
parseVar = UVar <$> parseIdent

parseApp :: Parser UntypedLC
parseApp = do
  o <- some parseULC
  pure $ foldl1 UApp o

parseNested :: Parser UntypedLC
parseNested = do
  single '('
  a <- parseULC
  single ')'
  pure a

parseLam :: Parser UntypedLC
parseLam = do
  single '\\'
  li <- some $ try (space *> parseIdent)
  try space
  single '.'
  space
  body <- parseULC
  pure $ foldr ULam body li

parseAndEval :: String -> IO ()
parseAndEval s = do
  maybe (putStrLn "expression malformed") runEval (parseMaybe parseULC s)

replU :: IO ()
replU = do
  putStr "UnypedLC>"
  hFlush stdout
  s <- getLine
  case s of
    "exit"   -> putStrLn "goodbye"
    "sample" -> sampleUntyped *> replU
    s'       -> parseAndEval s' *> replU

sampleUntyped :: IO ()
sampleUntyped = do
  runEval $ UApp (UApp andu truu) flsu
  runEval $ UApp iu (uvar "q")
  runEval $ (UApp skk (uvar "q"))
  where skk = UApp (UApp su ku) ku
