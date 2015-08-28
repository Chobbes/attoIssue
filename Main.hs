module Main where

import Data.Attoparsec.Text
import qualified Data.Text as T
import Control.Applicative
import Control.Monad.State
import Control.Monad.Trans.Except

type EvalParser = StateT Integer Parser

liftP :: Parser a -> EvalParser a
liftP = lift

parseOp :: Char -> Parser ()
parseOp c = do skipSpace
               char c
               skipSpace

addExpr :: EvalParser Double
addExpr = do val <- factor
             rest <- addExpr'
             return $ val + rest
             
addExpr' :: EvalParser Double
addExpr' = terms <|> return 0
  where terms :: EvalParser Double
        terms = do liftP $ parseOp '+'
                   l <- factor
                   r <- addExpr'
                   return $ l + r

factor :: EvalParser Double
factor = liftP double

parseEval :: EvalParser a -> String -> Either String (a, Integer)
parseEval p str = parseOnly (runStateT p 0) (T.pack str)

main :: IO ()
main = print $ parseEval addExpr "1 + 2"
