--Klaudia Algiz, 333811

module Interpreter where

import Absgrammar

import qualified Data.Map as Map
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe
import Data.Bool
import Data.List

maxLocation = 1000000

data Val = VInt Integer | VBool Bool | VFun Env [VarDec] [ValDec] Block 
	 | VRecord Env
	 deriving (Eq,Ord,Show)

type Loc = Integer

type Env = Map.Map Ident [Loc]
type Store = Map.Map Loc Val

type Eval a = ReaderT Env (ErrorT String (StateT Store IO)) a
runEval:: Env -> Store -> Eval a -> IO(Either String a, Store)
runEval env st ev = runStateT((runErrorT(runReaderT ev env))) st

runInterpreter :: Progr -> IO(Either String (), Store)
runInterpreter p = runEval (Map.empty) (Map.insert 0 (VInt 1) Map.empty) (runProgr p)

runProgr :: Progr -> Eval ()
runProgr (Program progrhead block) = do 
	  runBlock block

runBlock :: Block -> Eval ()
runBlock (PBlock decpart stms) = do
	  oldEnv <- ask
	  env <- makeDeclarations decpart oldEnv
	  local (const env) (mapM (runStm) stms)
	  return ()

getAllocation :: Eval Loc
getAllocation = do
	store <- get
	let locFree = Map.lookup 0 store
	when (isNothing locFree) (throwError ("Location not given"))
	let locFree1 = fromJust locFree
	case locFree1 of
		(VInt i) -> do
			let store1 = Map.insert 0 (VInt (i+1)) store
			put store1
			return i


createList :: Integer -> Eval [Loc]
createList 0 = return []
createList size = do
	loc <- getAllocation
	actList <- createList (size - 1) 
	return $ actList ++ [loc]

makeDeclarations :: DecPart -> Env -> Eval Env
makeDeclarations (DeclPart vardecs fundecs) env = do
	newEnv <- foldM makeVarDeclaration env vardecs
	newEnv1 <- foldM makeFunDeclaration newEnv fundecs
	return newEnv1


makeVarDeclaration :: Env -> VarDec -> Eval Env
makeVarDeclaration env (VarDecl id type1) = do
	case type1 of
		(TArray size _) -> do
			locations <- createList size
			return $ Map.insert id locations env
		(TRecord valdecs) -> do
			location <- createList 1
			recEnv <- foldM makeRecordValDeclaration Map.empty valdecs
			store <- get
			let store1 = Map.insert (location !! 0) (VRecord recEnv) store
			put store1
			return $ Map.insert id location env
		_ -> do
			location <- createList 1
			return $ Map.insert id location env

makeRecordValDeclaration :: Env -> ValDec -> Eval Env
makeRecordValDeclaration env (ValDecl id type1) = do
	location <-  createList 1
	return $ Map.insert id location env
	
makeFunDeclaration :: Env -> FunDec -> Eval Env
makeFunDeclaration env (FunDecl funhead block) =
	runFunHead funhead env block
	
runFunHead :: FunHead -> Env -> Block -> Eval Env
runFunHead (FunHeader id vardecs valdecs type1) env block = do
	location <- createList 1
	let newEnv1 = Map.insert id location env
	locRet <- createList 1
	let newEnv2 = Map.insert (Ident "return") locRet newEnv1
	store <- get
	let store1 = Map.insert (location !! 0) (VFun newEnv2 vardecs valdecs block) store
	put store1
	return newEnv1

runStm :: Stm -> Eval ()
runStm x = case x of
	StmBlock stms  -> do
		stms1 <- mapM (runStm) stms
		return ()
	AssignmentStm lval exp  -> do
		val <- runExp exp
		assign lval val
	IfStm exp stm1 stm2  -> do
		boolean <- runExp exp
		case boolean of
			(VBool True) -> runStm stm1
			(VBool False) -> runStm stm2
	WhileStm exp stm  -> do
		boolean <- runExp exp
		case boolean of
			(VBool True) -> do 
				runStm stm
				runStm x
			_ -> return ()
	ForStm stm1 exp2 stm2  -> do
		endVal <- runExp exp2
		case stm1 of
			(AssignmentStm lval exp1) -> do
				beginVal <- runExp exp1
				if beginVal <= endVal then do
					assign lval beginVal
					runStm stm2
					case beginVal of
						(VInt i) ->
							runStm (ForStm (AssignmentStm lval (EInt (i + 1))) exp2 stm2)
				else
					return ()
				
	PrintStm exp  -> do
		val <- runExp exp
		case val of
			(VInt i) -> do 
				liftIO $ print i
				return ()
			(VBool b) -> do 
				liftIO $ print b
				return ()
	ReturnStm exp  -> do
		env <- ask
		let retLocation = Map.lookup (Ident "return") env
		let retLoc = fromJust retLocation
		val <- runExp exp
		store <- get
		let store1 = Map.insert (retLoc !! 0) val store
		put store1
		
	Skip -> return ()

assign :: LVal -> Val -> Eval ()
assign x val = case x of
	(LValIdent id) -> do
		env <- ask
		let loc = Map.lookup id env
		let loc1 = fromJust loc
		store <- get
		let store1 = Map.insert (loc1 !! 0) val store
		put store1
	(LValArrayField id exp) -> do
		index1 <- runExp exp
		case index1 of
			(VInt index) -> do
				env <- ask
				let loc = Map.lookup id env
				let loc1 = fromJust loc
				if ((length loc1 >= fromInteger index) && (index > 0)) then do
					store <- get
					let store1 = Map.insert (loc1 !! fromInteger (index-1)) val store
					put store1
				else do
					throwError ("Array index out of bounds")
	(LValRecordField id1 id2) -> do
		env <- ask
		let loc1 = Map.lookup id1 env
		let loc11 = fromJust loc1
		store <- get
		let recEnv = Map.lookup (loc11 !! 0) store
		let recEnv1 = fromJust recEnv
		case recEnv1 of
			(VRecord recEnv2) -> do
				let loc2 = Map.lookup id2 recEnv2
				let loc22 = fromJust loc2
				let store1 = Map.insert (loc22 !! 0) val store
				put store1
		

runExp :: Exp -> Eval Val
runExp x = case x of
  EEqual exp1 exp2  -> do
	  val1 <- runExp exp1
	  val2 <- runExp exp2
	  return $ VBool (val1 == val2)

  ENotEqual exp1 exp2  -> do
	  val1 <- runExp exp1
	  val2 <- runExp exp2
	  return $ VBool (val1 /= val2)

  EGreaterThan exp1 exp2  -> do
	  val1 <- runExp exp1
	  val2 <- runExp exp2
	  return $ VBool (val1 > val2)

  ELowerThan exp1 exp2  -> do
	  val1 <- runExp exp1
	  val2 <- runExp exp2
	  return $ VBool (val1 < val2)

  EOr exp1 exp2  -> do
	  val1 <- runExp exp1
	  val2 <- runExp exp2
	  case (val1, val2) of
		(VBool b1, VBool b2) ->
			return $ VBool (b1 || b2)

  EAnd exp1 exp2  -> do
	  val1 <- runExp exp1
	  val2 <- runExp exp2
	  case (val1, val2) of
		(VBool b1, VBool b2) ->
			return $ VBool (b1 && b2)

  EAdd exp1 exp2  -> do
	  val1 <- runExp exp1
	  val2 <- runExp exp2
	  case (val1, val2) of
		(VInt i1, VInt i2) ->
			return $ VInt (i1 + i2)

  ESub exp1 exp2  -> do
	  val1 <- runExp exp1
	  val2 <- runExp exp2
	  case (val1, val2) of
		(VInt i1, VInt i2) ->
			return $ VInt (i1 - i2)

  EMul exp1 exp2  -> do
	  val1 <- runExp exp1
	  val2 <- runExp exp2
	  case (val1, val2) of
		(VInt i1, VInt i2) ->
			return $ VInt (i1 * i2)

  EDiv exp1 exp2  -> do
	  val1 <- runExp exp1
	  val2 <- runExp exp2
	  case (val1, val2) of
		(VInt i1, VInt i2) ->
			if i2 == 0 then
				throwError ("attempt to divide by zero")
			else
				return $ VInt (i1 `div` i2)

  EInt n  -> return $ VInt n
  EBool boolean  -> return $ VBool $ booleanToBool boolean
  EVar lval  -> runLValExp lval 
  EFuncCall id exps  -> do
	  runFunction id exps
  ELambda vardecs valdecs type1 block -> do
	env <- ask
	locRet <- createList 1
	let newEnv = Map.insert (Ident "return") locRet env
	return $ VFun newEnv vardecs valdecs block

runLValExp :: LVal -> Eval Val
runLValExp x = case x of
	LValIdent id  -> do
		env <- ask
		let loc1 = Map.lookup id env
		let loc2 = fromJust loc1
		store <- get
		let val = Map.lookup (loc2 !! 0) store
		when (isNothing val) (throwError ("uninitialized variable " ++ show id))
		return $ fromJust val

	LValArrayField id exp  -> do
		env <- ask
		val <- runExp exp
		let loc = Map.lookup id env
		let loc1 = fromJust loc
		case val of
			(VInt i) ->
				if (((length loc1) < fromInteger i) || (i <= 0)) then
					throwError ("array index out of bounds in " ++ show id)
				else do
					store <- get
					let val1 = Map.lookup (loc1 !! fromInteger (i-1)) store
					when (isNothing val1) (throwError ("uninitialized variable " ++ 
						show id ++ "[" ++ show i ++ "]"))
					return $ fromJust val1
					
	LValRecordField id1 id2 -> do
		env <- ask
		let loc1 = Map.lookup id1 env
		let loc11 = fromJust loc1
		store <- get
		let recEnv = Map.lookup (loc11 !! 0) store
		let recEnv1 = fromJust recEnv
		case recEnv1 of
			(VRecord recEnv2) -> do
				let loc2 = Map.lookup id2 recEnv2
				let loc22 = fromJust loc2
				let val = Map.lookup (loc22 !! 0) store
				when (isNothing val) (throwError ("uninitialized field " ++ show id2 ++ " in record " ++ show id1))
				return $ fromJust val

runFunction :: Ident -> [Exp] -> Eval Val
runFunction id args = do
	env <- ask
	let funLoc =  Map.lookup id env
	let funLoc1 = fromJust funLoc
	store <- get
	let fun = Map.lookup (funLoc1 !! 0) store
	let fun1 = fromJust fun
	case fun1 of
		(VFun funEnv varDecs valDecs block) -> do
			let varArgs = take (length varDecs) args
			let valArgs = drop (length varDecs) args
			funEnv <- makeFunVarDeclarations funEnv varDecs varArgs
			valArgsVal <- mapM (runExp) valArgs
			funEnv <- makeFunValDeclarations funEnv valDecs valArgsVal
			local (const funEnv) (runBlock block)
			let returnLoc = Map.lookup (Ident "return") funEnv
			let returnLoc1 = fromJust returnLoc
			store <- get
			let returnVal = Map.lookup (returnLoc1 !! 0) store
			case returnVal of
				Nothing -> throwError ("function " ++ show id ++ " does not return a value")
				Just returnVal1 -> return returnVal1

makeFunVarDeclarations :: Env -> [VarDec] -> [Exp] -> Eval Env
makeFunVarDeclarations env [] [] =  return env
makeFunVarDeclarations funEnv (varDec : varDecs) (exp : exps) =
	case (varDec) of
		(VarDecl id _) -> case exp of
			(EVar lval) -> case lval of
				(LValIdent id1) -> do
					env <- ask
					let location = Map.lookup id1 env
					let loc = fromJust location
					return $ Map.insert id loc funEnv
				(LValArrayField id1 exp1) -> do
					val <- runExp exp1
					env <- ask
					let location = Map.lookup id1 env
					let loc = fromJust location
					case val of
						(VInt i) ->
							if (((length loc) < fromInteger i) || (i <= 0)) then
								throwError ("array index out of bounds in " ++ show id1)
							else do
								return $ Map.insert id [(loc !! fromInteger(i-1))] funEnv
				(LValRecordField id1 id2) -> do
					env <- ask 
					let loc1 = Map.lookup id1 env
					let loc11 = fromJust loc1
					store <- get
					let recEnv = Map.lookup (loc11 !! 0) store
					let recEnv1 = fromJust recEnv
					case recEnv1 of
						VRecord recEnv2 -> do
							let loc2 = Map.lookup id2 recEnv2
							let loc22 = fromJust loc2
							return $ Map.insert id loc22 funEnv
					
			_ -> do
				val <- runExp exp
				location <- createList 1
				store <- get
				let store1 = Map.insert (location !! 0) val store
				put store1
				return $ Map.insert id location funEnv


makeFunValDeclarations :: Env -> [ValDec] -> [Val] -> Eval Env
makeFunValDeclarations env [] [] = return env
makeFunValDeclarations env (valDec : valDecs) (val : vals) = do
	location <- createList 1
	case valDec of
		(ValDecl id _) -> do 
			store <- get
			let store1 = Map.insert (location !! 0) val store
			put store1
			return $ Map.insert id location env

booleanToBool :: Boolean -> Bool
booleanToBool b = case b of
	(BoolTrue) -> True
	(BoolFalse) -> False