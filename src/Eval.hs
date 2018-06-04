{-# LANGUAGE FlexibleContexts #-}
module Eval (evaluate, run) where

import Parser
import Expr

import Text.Parsec
import Control.Monad.State
import Control.Monad.Except (MonadError, throwError)
import qualified Data.Map as M

-- Evaluation
evaluate :: Expr -> Either String Val
evaluate expr = evalStateT (eval expr) M.empty

run :: String -> Either String Val
run s = case parseProgram s of
  Left perror -> throwError $
    "ParseError (" ++ show (sourceLine pos) ++ ":" ++ show (sourceColumn pos) ++ ")"
    where pos = errorPos perror
  Right expr  -> evaluate expr

eval :: (MonadError String m, MonadState Env m) => Expr -> m Val
eval (I n) = pure (VInt n)
eval Nil = pure (VList [])

eval (Cons x xs) = do
  x'  <- eval x
  xs' <- eval xs >>= val2List
  pure $ VList (x':xs')

eval (ListUnOp _ f xs) = f <$> (eval xs >>= val2List)
eval (BinOp _ op x y) = evalIntBinOp op x y
eval (UnOp _ op x) = evalIntUnOp op x

eval (If p a b) = do
  p' <- eval p >>= val2Bool
  if p' then eval a else eval b

eval (Var v) = do
  e <- get
  case M.lookup v e of
    Just val -> pure val
    Nothing  -> throwError $ "Undefined variable: " ++ v

eval (Let attribs body) = evalAttribsWith evalAttribs attribs body
eval (LetStar attribs body) = evalAttribsWith evalAttribsRec attribs body

eval (Unpack names expr@(Cons _ _) body) = do
  vals <- cons2List expr
  if length names == length vals
    then evalAttribsWith evalAttribs (names `zip` vals) body
    else throwError "TypeError: can't unpack with different lengths."

eval (Unpack _ Nil _) =
  throwError "Error: can't unpack an empty list"

eval (Unpack _ badExpr _) =
  throwError $ "TypeError: not a list: " ++ show badExpr

eval (Proc vars body) = gets (VProc vars body)

eval (LetProc procs inbody) =
  eval $ LetStar
  [(name, Proc vars funbody) | (name, vars, funbody) <- procs]
  inbody

-- TODO: clean this imperative mess
eval (Apply procName args) = do
  proc' <- gets (M.lookup procName)
  case proc' of
    Just (VProc vars body procScope) ->
      if length vars == length args
      then do
        argVals <- traverse eval args
        oldState <- get
        modify (extendEnv procName (VProc vars body procScope))
        evalArgs vars argVals
        eval body <* put oldState
      else throwError
           $ "Error: expected " ++ show (length vars) ++ " arguments, got "
           ++ show (length args) ++ ": " ++ procName
    _ -> throwError $ "TypeError: not a procedure: " ++ procName

-- eval utils
evalArgs :: (MonadState Env m) => [String] -> [Val] -> m ()
evalArgs names vals =
  sequence_ $ (\(name, val) -> modify (extendEnv name val)) <$> (names `zip` vals)

evalAttribsWith :: (MonadError String m, MonadState Env m)
                => ([(String, Expr)] -> m ()) -> [(String, Expr)] -> Expr -> m Val
evalAttribsWith evalFun attribs body = do
  oldState <- get
  evalFun attribs
  bodyVal <- eval body
  put oldState
  pure bodyVal

evalAttribs :: (MonadError String m, MonadState Env m) => [(String, Expr)] -> m ()
evalAttribs [] = pure mempty
evalAttribs ((name, expr) : vs) = do
  val <- eval expr
  evalAttribs vs
  modify (extendEnv name val)

evalAttribsRec :: (MonadError String m, MonadState Env m) => [(String, Expr)] -> m ()
evalAttribsRec [] = pure mempty
evalAttribsRec ((name, expr) : vs) = do
  val <- eval expr
  modify (extendEnv name val)
  evalAttribsRec vs

cons2List :: MonadError String m => Expr -> m [Expr]
cons2List Nil = pure []
cons2List (Cons x xs) = (x:) <$> cons2List xs
cons2List x = throwError $ "TypeError: not a list: " ++ show x

evalIntUnOp :: (MonadError String m, MonadState Env m)
            => (Int -> Val) -> Expr -> m Val
evalIntUnOp f x = f <$> (eval x >>= val2Int)

evalIntBinOp :: (MonadError String m, MonadState Env m)
             => (Int -> Int -> Val) -> Expr -> Expr -> m Val
evalIntBinOp op x y = op <$> (eval x >>= val2Int) <*> (eval y >>= val2Int)

extendEnv :: String -> Val -> Env -> Env
extendEnv = M.insert
