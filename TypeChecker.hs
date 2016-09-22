-- Klaudia Algiz, 333811

module TypeChecker where

import Absgrammar

import qualified Data.Map as Map
import Control.Monad.Error
import Control.Monad.Reader
import Data.Maybe

data TypeVar = TypeG Type | Record Env
	 deriving (Eq,Ord,Show)

type Env = Map.Map Ident TypeVar

type Eval a = ReaderT Env (ErrorT String IO) a
runEval:: Env -> Eval a -> IO(Either String a)
runEval env ev = runErrorT(runReaderT ev env)

runTypeChecker :: Progr -> IO(Either String ())
runTypeChecker p = runEval (Map.empty) (transProgr p)

transProgr :: Progr -> Eval ()
transProgr (Program progrhead block) = do 
	  transBlock block
	  return ()
  
transBlock :: Block -> Eval ()
transBlock (PBlock decpart stms) = do
	  oldEnv <- ask
	  env <- makeDeclarations decpart oldEnv
	  local (const env) (mapM (transStm) stms)
	  return ()

makeDeclarations :: DecPart -> Env -> Eval Env
makeDeclarations (DeclPart vardecs fundecs) env = do
	newEnv <- foldM makeVarDeclaration env vardecs
	newEnv1 <- foldM makeFunDeclaration newEnv fundecs
	return newEnv1
	  
makeVarDeclaration :: Env -> VarDec -> Eval Env
makeVarDeclaration env (VarDecl id type1) =
	case type1 of
		(TRecord valdecs) -> do
			newEnv <- foldM makeValDeclarationInRecord Map.empty valdecs
			return $ Map.insert id (Record newEnv) env
		_ -> return $ Map.insert id (TypeG type1) env

	
makeValDeclarationInRecord :: Env -> ValDec -> Eval Env
makeValDeclarationInRecord env (ValDecl id type1) = do
	case type1 of
		(TArray _ _) -> throwError ("record field cannot be an array " ++ show id)
		(TRecord _) -> throwError ("record field cannot be of record type " ++ show id)
		_ -> return $ Map.insert id (TypeG type1) env
	
makeValDeclaration :: Env -> ValDec -> Eval Env
makeValDeclaration env (ValDecl id type1) = do
	case type1 of
		(TArray _ _) -> throwError ("function parameter cannot be an array " ++ show id)
		(TRecord _) -> throwError ("function parameter cannot be of record type " ++ show id)
		_ -> return $ Map.insert id (TypeG type1) env
	
makeFunDeclaration :: Env -> FunDec -> Eval Env
makeFunDeclaration env (FunDecl funhead block) = do
	newEnv <- transFunHead funhead env
	local (const newEnv) (transBlock block)
	return newEnv
	

transFunHead :: FunHead -> Env -> Eval Env
transFunHead (FunHeader id vardecs valdecs type1) env = do
	newEnv <- foldM makeVarDeclaration env vardecs
	newEnv1 <- foldM makeValDeclaration newEnv valdecs
	let newEnv2 = Map.insert (Ident "return") (TypeG type1) newEnv1
	let types = (map getTypeFromVarDec vardecs) ++ (map getTypeFromValDec valdecs)
	return $ Map.insert id (TypeG (TFunction types type1)) newEnv2

getTypeFromVarDec :: VarDec -> Type
getTypeFromVarDec (VarDecl id type1) = type1

getTypeFromValDec :: ValDec -> Type
getTypeFromValDec (ValDecl id type1) = type1

transExp :: Exp -> Eval TypeVar
transExp x = case x of
  EEqual exp1 exp2  -> do
	  type1 <- transExp exp1
	  type2 <- transExp exp2
	  case (type1, type2) of
		(TypeG TInt, TypeG TInt) -> 
			return $ TypeG TBool
		(TypeG TBool, TypeG TBool) ->
			return $ TypeG TBool
		_ -> throwError ("wrong type in " ++ show x)

  ENotEqual exp1 exp2  -> do
	type1 <- transExp exp1
	type2 <- transExp exp2
	case (type1, type2) of
		(TypeG TInt, TypeG TInt) -> 
			return $ TypeG TBool
		(TypeG TBool, TypeG TBool) ->
			return $ TypeG TBool
		_ -> throwError ("wrong type in " ++ show x)

  EGreaterThan exp1 exp2  -> do
	  type1 <- transExp exp1
	  type2 <- transExp exp2
	  case (type1, type2) of
		(TypeG TInt, TypeG TInt) ->
			return $ TypeG TBool
		_ -> throwError("wrong type in " ++ show x)

  ELowerThan exp1 exp2 -> do
	  type1 <- transExp exp1
	  type2 <- transExp exp2
	  case (type1, type2) of
		(TypeG TInt, TypeG TInt) ->
			return $ TypeG TBool
		_ ->throwError("wrong type in " ++ show x)
  EAnd exp1 exp2  -> do
	  type1 <- transExp exp1
	  type2 <- transExp exp2
	  case (type1, type2) of
		(TypeG TBool, TypeG TBool) ->
			return $ TypeG TBool
		_ -> throwError("wrong type in " ++ show x)
		
  EOr exp1 exp2 -> do
	  type1 <- transExp exp1
	  type2 <- transExp exp2
	  case (type1, type2) of
		(TypeG TBool, TypeG TBool) ->
			return $ TypeG TBool
		_ -> throwError("wrong type in " ++ show x)

  EAdd exp1 exp2 -> checkArithmeticExp x exp1 exp2
  ESub exp1 exp2 -> checkArithmeticExp x exp1 exp2
  EMul exp1 exp2 -> checkArithmeticExp x exp1 exp2
  EDiv exp1 exp2 -> checkArithmeticExp x exp1 exp2
  EInt n  -> return $ TypeG TInt
  EVar lval  -> checkLValExp x lval 
  EBool boolean  -> return $ TypeG TBool
  EFuncCall id exps  -> do
	  expTypes <- mapM (transExp) exps
	  env <- ask
	  let type1 = Map.lookup id env
	  when (isNothing type1) (throwError ("unknown function " ++ show id))
	  let type2 = fromJust type1
	  case type2 of
		(TypeG (TFunction argTypes typeF)) -> do
			let expTypes1 = map (\(TypeG x) -> x) expTypes
			if expTypes1 == argTypes then 
				return $ TypeG typeF
			else 
				throwError ("arguments of function " ++ show id ++ " have inproper type")
		_ -> throwError("variable " ++ show id ++ " is not a function")
  ELambda vardecs valdecs type1 block  -> do
	env <- ask
	newEnv <- foldM makeVarDeclaration env vardecs
	newEnv1 <- foldM makeValDeclaration newEnv valdecs
	let newEnv2 = Map.insert (Ident "return") (TypeG type1) newEnv1
	let types = (map getTypeFromVarDec vardecs) ++ (map getTypeFromValDec valdecs)
	local (const newEnv2) (transBlock block)
	return $ TypeG $ TFunction types type1
	  

checkArithmeticExp :: Exp -> Exp -> Exp -> Eval TypeVar
checkArithmeticExp x exp1 exp2 = do
	type1 <- transExp exp1
	type2 <- transExp exp2
	case (type1, type2) of
		(TypeG TInt, TypeG TInt) ->
			return $ TypeG TInt
		_ -> throwError ("wrong type in " ++ show x)
		
checkLValExp :: Exp -> LVal -> Eval TypeVar
checkLValExp x lval = case lval of
	LValIdent id  -> do
		env <- ask
		let type1 = Map.lookup id env
		when (isNothing type1) (throwError ("unknown variable " ++ show id))
		let type2 = fromJust type1
		case type2 of
			(TypeG TInt) -> 
				return $ type2
			(TypeG TBool) ->
				return $ type2
			(TypeG (TFunction _ _)) ->
				return type2
			_ -> throwError ("wrong type " ++ show(id))

	LValArrayField id exp  -> do
		env <- ask
		let type1 = Map.lookup id env
		when (isNothing type1) (throwError ("unknown variable " ++ show id))
		let type2 = fromJust type1
		case type2 of
			(TypeG (TArray _ typeA)) -> do
				val <- transExp exp
				case val of
					(TypeG TInt) -> return $ TypeG typeA
					_ -> throwError("index array not an integer " ++ show(id))
			_ -> throwError ("variable " ++ show(id) ++ "is not an array")
			
	LValRecordField id1 id2  -> do
		env <- ask
		let type1 = Map.lookup id1 env
		when (isNothing type1) (throwError ("unknown variable " ++ show id1))
		let type2 = fromJust type1
		case type2 of
			(Record recEnv) -> do
				let recFieldType = Map.lookup id2 recEnv
				when (isNothing recFieldType) (throwError ("unknown field " ++ show id2 ++ " of record " ++ show id1))
				let recFieldType1 = fromJust recFieldType
				return recFieldType1
			_ -> throwError ("variable " ++ show id1 ++ "is not of record type.")


transStm :: Stm -> Eval ()
transStm x = case x of
  StmBlock stms  -> do
	  stms1 <- mapM (transStm) stms
	  return ()
  AssignmentStm lval exp  -> do 
	  lvalType <- transExp $ EVar lval
	  expType <- transExp exp
	  if (lvalType == expType) then
		return ()
	  else
		throwError("incompatible type in " ++ show(x))
  IfStm exp stm1 stm2  -> do
	  typeExp <- transExp exp
	  unless (typeExp == (TypeG TBool)) (throwError $ "if condition not of type bool in " ++ show(x))
	  transStm stm1
	  transStm stm2
	  return ()

  WhileStm exp stm  -> do
	  typeExp <- transExp exp
	  unless (typeExp == (TypeG TBool)) (throwError $ "if condition not of type bool in " ++ show(x))
	  transStm stm
	  return ()

  ForStm stm1 exp stm2  -> do
	  case (stm1) of
		(AssignmentStm lval exp1) -> do
			typeExp1 <- transExp exp1
			unless (typeExp1 == (TypeG TInt)) (throwError $ "wrong type in for statement " ++ show(x))
			case lval of
				(LValIdent id) -> do
					typeExp <- transExp exp
					unless (typeExp == (TypeG TInt)) (throwError $ "wrong type in for statement " ++ show(x))
					transStm stm1
					transStm stm2
					return ()
				_ -> throwError $ "wrong type in for statement " ++ show(x)
		_ -> throwError $ "lack of assignment statement in " ++ show x
	  
  PrintStm exp  -> do
	  typeExp <- transExp exp
	  case typeExp of
		(TypeG TInt) -> return ()
		(TypeG TBool) -> return ()
		_ -> throwError("wrong type of print argument in " ++ show(x))
		
  ReturnStm exp  -> do
	  typeExp <- transExp exp
	  env <- ask
	  let type1 = Map.lookup (Ident "return") env
	  case type1 of
		Nothing -> throwError ("return statement out of function body")
		Just type2 -> do
			if type2 == typeExp then
				return ()
			else
				throwError ("incompatible type in return statement " ++ show x)
				
  Skip -> return ()
