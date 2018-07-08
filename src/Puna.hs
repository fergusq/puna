module Puna where

import System.IO (isEOF)

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Identity

import Coroutine
import Parser

import Data.Maybe
import qualified Data.Map as M

-- | Prelexer converts a list of characters to a list of 'LNToken Char'

prelex :: String -> [LNToken Char]
prelex s = prelex' s 1 1

prelex' []        _ _ = []
prelex' ('\n':cs) l k = LNToken '\n' l k : prelex' cs (l+1) 1
prelex' (c   :cs) l k = LNToken c    l k : prelex' cs l     (k+1)

-- Lexer

type TParser r = ParserT (LNToken Char) Identity r

tokenp :: TParser (LNToken String)
tokenp = identifier <|> acceptL "->" <|> acceptL "=>" <|> regexp <|> quotep <|> (((:"")<$>) <$> oneOfL "=()|.,;$:")

quotep :: TParser (LNToken String)
quotep = acceptL "\"" *>
         ((('"':)<$>) <$> lnToken (many $ (noneOfL "\\\"" <|> (oneOfL "\\" *> oneOfL "\"\\"))))
         <* acceptL "\""

regexp :: TParser (LNToken String)
regexp = acceptL ":/" *>
         ((('/':)<$>) <$> lnToken (many $ noneOfL "/\\" <|> (oneOfL "\\" *> oneOfL "/\\")))
         <* acceptL "/"

tokensp :: String -> TParser [LNToken String]
tokensp eof = many (many space *> tokenp) <* many space <* acceptL eof

-- Parser

data PExpr
    = PIdent String
    | PString String
    | PCall String [PExpr]
    | PPipe PExpr PExpr
    | PFor [(PPattern, PExpr)]
    | PPull [(PPattern, PExpr)]
    | PLet PPattern PExpr PExpr
    | PBlock [PExpr]
    deriving (Show)

data PPattern
    = PPVar String PPattern
    | PPString String
    | PPAnyString
    deriving (Show)

type PParser r = ParserT (LNToken String) Identity r

programp :: [String] -> PParser PExpr
programp eof = expressionp <* acceptL ["."] <* acceptL eof

expressionp :: PParser PExpr
expressionp = do e <- pipep
                 es <- many (acceptL [";"] *> expressionp)
                 return $ PBlock (e:es)

pipep :: PParser PExpr
pipep = do e <- callp <|> forp <|> pullp <|> letp <|> identp <|> parentp <|> stringp
           es <- many (acceptL ["|"] *> pipep)
           return $ foldl PPipe e es

identp :: PParser PExpr
identp = PIdent <$> identifierL

stringp :: PParser PExpr
stringp = do s <- nextToken
             unless ((content s !! 0) == '\"') $
                 parsingError s "string token"
             return . PString $ drop 1 (content s)

callp :: PParser PExpr
callp = PCall <$> identifierL <*> (acceptL ["("] *> argumentsp) <* acceptL [")"]

argumentsp :: PParser [PExpr]
argumentsp = (do a <- expressionp
                 as <- many (acceptL [","] *> expressionp)
                 return (a:as))
           <|> nothing

forp :: PParser PExpr
forp = PFor <$> some ((,) <$> patternp <*> (acceptL ["=>"] *> expressionp))

pullp :: PParser PExpr
pullp = PPull <$> some ((,) <$> patternp <*> (acceptL ["->"] *> expressionp))

letp :: PParser PExpr
letp = PLet <$> patternp <*> (acceptL ["="] *> pipep) <*> (acceptL [";"] *> expressionp)

patternp :: PParser PPattern
patternp = varpp <|> stringpp <|> anystringpp

varpp :: PParser PPattern
varpp = PPVar <$> identifierL <*> patternp

stringpp :: PParser PPattern
stringpp = do (PString s) <- stringp
              return $ PPString s

anystringpp :: PParser PPattern
anystringpp = acceptL ["$"] >> return PPAnyString

parentp :: PParser PExpr
parentp = (acceptL ["("] *> expressionp) <* acceptL [")"]

-- Parser function

lexPuna :: String -> Either [ParsingError] [LNToken String]
lexPuna code = runIdentity $ parse (tokensp [tEofChar]) (prelex $ code++[tEofChar])
    where
        tEofChar = '\0'

parsePuna :: String -> Either [ParsingError] PExpr
parsePuna code = do tokens <- lexPuna code
                    runIdentity $ parse (programp [pEofStr]) (tokens++[pEof])
                 where
                     pEofStr = "<EOF>"
                     pEof = LNToken pEofStr 0 0

-- Compiler

type PunaValue = String
type PunaScope = M.Map String PunaValue

type PunaFunc = PunaScope -> StreamT PunaValue IO ()

eval :: PunaFunc -> PunaScope -> (PunaValue -> StreamT PunaValue IO ()) -> StreamT PunaValue IO ()
eval f scope g = pipePipe [f scope, pullPipe >>= \(Just v) -> g v]

compile :: PExpr -> PunaFunc
compile (PString string) scope = pushPipe string
compile (PIdent ident)   scope = pushPipe . fromJust $ M.lookup ident scope <|> Just ""
compile (PPipe e1 e2)    scope = pipePipe [compile e1 scope, compile e2 scope]

compile (PFor []) scope = return ()
compile (PFor pairs) scope
    = let g _ []                   = return False
          g x ((pattern, expr):ps) = case x >>= patternToInserts pattern of
                                          Just is -> compile expr (foldr ($) scope is) >> return True
                                          Nothing -> g x ps
          f = do x <- pullPipe
                 r <- g x pairs
                 when r f
      in f

compile (PPull []) scope = error ("pattern failed")
compile (PPull ((pattern, expr):ps)) scope
    = do x <- pullPipe
         case x >>= patternToInserts pattern of
             Just is -> compile expr (foldr ($) scope is)
             Nothing -> compile (PFor ps) scope

compile (PLet pattern val expr) scope
    = eval (compile val) scope $ \v ->
      let (Just is) = patternToInserts pattern v
      in compile expr (foldr ($) scope is)

compile (PBlock exprs) scope
    = let (v:vs) = ($ scope) <$> map compile exprs
      in foldr (>>) v vs

compile (PCall "add" [a, b]) scope = eval (compile a) scope $ \x ->
                                     eval (compile b) scope $ \y ->
                                     pushPipe (x++y)
compile (PCall f as)         scope = error ("unknown function " ++ f)

-- Pattern

patternToInserts :: PPattern -> PunaValue -> Maybe [PunaScope -> PunaScope]
patternToInserts (PPVar v p) e = do inserts <- patternToInserts p e
                                    return [M.insert v e]

patternToInserts (PPString s) e = if s == e then Just [] else Nothing
-- patternToInserts (PPString _) _ = Nothing

patternToInserts PPAnyString _ = Just []
-- patternToInserts PPAnyString _ = Nothing

-- Runtime

inputStream :: CoroutineT PunaValue IO ()
inputStream = do done <- lift isEOF
                 unless done $ do
                     line <- lift getLine
                     push line
                     inputStream

outputStream :: CoroutineT PunaValue IO () -> IO ()
outputStream input = do p <- pull input
                        case p of
                            Just (s, l) -> do putStrLn s
                                              outputStream l
                            Nothing -> return ()

runProgram :: String -> IO ()
runProgram code = case program of
                      Left error -> print error
                      Right io -> io
    where
        program = do expr <- parsePuna code
                     let func = compile expr
                     let scope = M.empty
                     return $ outputStream (() <$ pipe (NoCR <$ inputStream) (() <$ func scope))
