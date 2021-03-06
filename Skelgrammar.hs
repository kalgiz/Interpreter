module Skelgrammar where

-- Haskell module generated by the BNF converter

import Absgrammar
import ErrM
type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transIdent :: Ident -> Result
transIdent x = case x of
  Ident str  -> failure x


transProgr :: Progr -> Result
transProgr x = case x of
  Program progrhead block  -> failure x


transProgrHead :: ProgrHead -> Result
transProgrHead x = case x of
  ProgramHeading id  -> failure x


transBlock :: Block -> Result
transBlock x = case x of
  PBlock decpart stms  -> failure x


transDecPart :: DecPart -> Result
transDecPart x = case x of
  DeclPart vardecs fundecs  -> failure x


transVarDec :: VarDec -> Result
transVarDec x = case x of
  VarDecl id type'  -> failure x


transValDec :: ValDec -> Result
transValDec x = case x of
  ValDecl id type'  -> failure x


transFunDec :: FunDec -> Result
transFunDec x = case x of
  FunDecl funhead block  -> failure x


transFunHead :: FunHead -> Result
transFunHead x = case x of
  FunHeader id vardecs valdecs type'  -> failure x


transType :: Type -> Result
transType x = case x of
  TInt  -> failure x
  TBool  -> failure x
  TArray n type'  -> failure x
  TRecord valdecs  -> failure x
  TFunction types type'  -> failure x


transStm :: Stm -> Result
transStm x = case x of
  StmBlock stms  -> failure x
  AssignmentStm lval exp  -> failure x
  IfStm exp stm0 stm  -> failure x
  WhileStm exp stm  -> failure x
  ForStm stm0 exp stm  -> failure x
  PrintStm exp  -> failure x
  ReturnStm exp  -> failure x
  Skip  -> failure x


transExp :: Exp -> Result
transExp x = case x of
  EEqual exp0 exp  -> failure x
  ENotEqual exp0 exp  -> failure x
  EGreaterThan exp0 exp  -> failure x
  ELowerThan exp0 exp  -> failure x
  EAnd exp0 exp  -> failure x
  EOr exp0 exp  -> failure x
  EAdd exp0 exp  -> failure x
  ESub exp0 exp  -> failure x
  EMul exp0 exp  -> failure x
  EDiv exp0 exp  -> failure x
  EInt n  -> failure x
  EVar lval  -> failure x
  EBool boolean  -> failure x
  EFuncCall id exps  -> failure x
  ELambda vardecs valdecs type' block  -> failure x


transLVal :: LVal -> Result
transLVal x = case x of
  LValIdent id  -> failure x
  LValArrayField id exp  -> failure x
  LValRecordField id0 id  -> failure x


transBoolean :: Boolean -> Result
transBoolean x = case x of
  BoolTrue  -> failure x
  BoolFalse  -> failure x



