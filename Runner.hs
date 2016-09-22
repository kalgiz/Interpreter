--Klaudia Algiz,333811

-- automatically generated by BNF Converter
module Main where


import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )

import Lexgrammar
import Pargrammar
import Printgrammar
import Absgrammar
import TypeChecker
import Interpreter

import ErrM

type ParseFun a = [Token] -> Err a

myLLexer = myLexer

type Verbosity = Int

--putStrV :: Verbosity -> String -> IO ()
putStrV v s = if v > 1 then putStrLn s else return ()

--runFile :: (Print a, Show a) => Verbosity -> ParseFun a -> FilePath -> IO ()
runFile v p f = putStrLn f >> readFile f >>= run v p

--run :: (Print a, Show a) => Verbosity -> ParseFun a -> String -> IO ()
run v p s = let ts = myLLexer s in case p ts of
           Bad s    -> do putStrLn "\nParse Failed...\n"
                          putStrLn s
           Ok  tree -> do putStrLn "\nParse Successful!"
                          checkResult <- runTypeChecker tree
                          case checkResult of
			       Left msg -> putStrLn msg
			       Right a -> do
					result <- runInterpreter tree
					case result of
						(res, store) ->
							case res of
								Left msg -> putStrLn msg
								Right a -> return ()

--main :: IO ()
main = do args <- getArgs
          case args of
            [] -> hGetContents stdin >>= run 2 pProgr
            "-s":fs -> mapM_ (runFile 0 pProgr) fs
            fs -> mapM_ (runFile 2 pProgr) fs




