{-# LANGUAGE FlexibleContexts #-}
module Expr (Expr (..), Val (..), Env, val2Int, val2Bool, val2List) where

import Data.List (intercalate)
import Control.Monad.Except (MonadError, throwError)
import qualified Data.Map as M

type Env = M.Map String Val

-- Expressions

data Expr = I        Int
          | BinOp    String (Int -> Int -> Val) Expr Expr
          | UnOp     String (Int -> Val) Expr
          | Cons     Expr Expr
          | ListUnOp String ([Val] -> Val) Expr
          | Nil
          | If       Expr Expr Expr
          | Var      String
          | Let      [(String, Expr)] Expr
          | LetStar  [(String, Expr)] Expr
          | Unpack   [String] Expr Expr
          | Proc     [String] Expr
          | Apply    String [Expr]
          | LetProc  String [String] Expr Expr

instance Show Expr where
  show (I i) = show i
  show (BinOp sym _ a b) = showBinOp sym a b
  show (UnOp sym _ x) = showUnOp sym x
  show (ListUnOp sym _ x) = showUnOp sym x
  show c@(Cons _ _) = "list(" ++ showElems c ++ ")"
  show Nil = "emptylist"
  show (If p t e) = "if " ++ show p ++ " then " ++ show t ++ " else " ++ show e
  show (Var v) = v
  show (Let vs body) =
    "let " ++ showAttribs vs ++ " in " ++ show body
  show (LetStar vs body) =
    "let* " ++ showAttribs vs ++ " in " ++ show body
  show (Unpack names expr body) =
    "unpack " ++ unwords names ++ " = " ++ show expr ++ " in " ++ show body
  show (Proc vars body) = "proc (" ++ intercalate "," vars ++ ") " ++ show body
  show (Apply func args) =
    func ++ "(" ++ intercalate "," (show <$> args) ++ ")"
  show (LetProc name vars funbody inbody) =
    "letproc " ++ name ++ " (" ++ intercalate "," vars ++ ") = " ++ show funbody ++ " in " ++ show inbody

showAttribs :: [(String, Expr)] -> String
showAttribs = unwords . fmap (\(name, expr) -> name ++ " = " ++ show expr)

showElems :: Expr -> String
showElems (Cons x Nil) = show x
showElems (Cons x xs) = show x ++ ", " ++ showElems xs
showElems _ = ""

showBinOp :: String -> Expr -> Expr -> String
showBinOp sym a b = sym ++ "(" ++ show a ++ ", " ++ show b ++ ")"

showUnOp :: String -> Expr -> String
showUnOp sym e = sym ++ "(" ++ show e ++ ")"

-- Values

data Val = VInt  Int
         | VBool Bool
         | VList [Val]
         | VProc [String] Expr Env

instance Show Val where
  show (VInt i) = show i
  show (VBool b) = show b
  show (VList xs) = show xs
  show (VProc vars body _) = "proc (" ++ intercalate "," vars ++ ") " ++ show body

val2Int :: (MonadError String m) => Val -> m Int
val2Int (VInt n) = pure n
val2Int x = typeError "Int" (show x)

val2Bool :: (MonadError String m) => Val -> m Bool
val2Bool (VBool b) = pure b
val2Bool x = typeError "Bool" (show x)

val2List :: (MonadError String m) => Val -> m [Val]
val2List (VList b) = pure b
val2List x = typeError "List" (show x)

typeError :: MonadError String m => String -> String -> m a
typeError expected obtained =
  throwError $ "TypeError: expected " ++ expected ++ ", got " ++ obtained
