module Puna where

import System.IO (isEOF)

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Identity
import Control.Monad.State

import Coroutine
import Parser

import Data.Maybe
import Data.List
import qualified Data.Map as M
import Text.Regex

-- | Prelexer converts a list of characters to a list of 'LNToken Char'

prelex :: String -> [LNToken Char]
prelex s = prelex' s 1 1

prelex' []        _ _ = []
prelex' ('\n':cs) l k = LNToken '\n' l k : prelex' cs (l+1) 1
prelex' (c   :cs) l k = LNToken c    l k : prelex' cs l     (k+1)

-- Lexer

type TParser r = ParserT (LNToken Char) Identity r

tokenp :: TParser (LNToken String)
tokenp = lnToken (some (oneOfL "0123456789"))
       <|> identifier
       <|> acceptL "->"
       <|> acceptL "=>"
       <|> acceptL ".."
       <|> acceptL "</"
       <|> acceptL "/>"
       <|> acceptL "</>"
       <|> regexp
       <|> quotep
       <|> (((:"")<$>) <$> oneOfL "=()|.,;$:#+-*/<>")

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
    | PInteger Integer
    | PObject String (M.Map String PExpr) [PExpr]
    | PCall String [PExpr]
    | PPipe PExpr PExpr
    | PFor [(PPattern, PExpr)]
    | PPull [(PPattern, PExpr)]
    | PLet PPattern PExpr PExpr
    | PBlock [PExpr]
    | PEmpty

instance Show PExpr where
    show (PIdent s) = s
    show (PString s) = show s
    show (PInteger i) = show i
    show (PObject t m l) = showObj t m l show
    show (PCall s es) = s ++ "(" ++ intercalate ", " (map show es) ++ ")"
    show (PPipe a b) = show a ++ " | " ++ show b
    show (PFor ps) = intercalate " " $ map (\(p, e) -> show p ++ " => (" ++ show e ++ ")") ps
    show (PPull ps) = intercalate " " $ map (\(p, e) -> show p ++ " -> (" ++ show e ++ ")") ps
    show (PLet pp a b) = show pp ++ " = " ++ show a ++ "; " ++ show b
    show (PBlock es) = intercalate "; " $ map show es
    show PEmpty = "()"

showObj :: String -> (M.Map String t) -> [t] -> (t -> String) -> String
showObj t m l showChild =
    "<" ++ t
    ++ concatMap (\(k, v) -> " " ++ k ++ "=" ++ showChild v) (M.toList m)
    ++ case length l of
        0 -> "/>"
        _ -> ">" ++ concatMap showChild l ++ "</" ++ t ++ ">"

data PPattern
    = PPVar String PPattern
    | PPString String
    | PPRegex Regex
    | PPAnyString
    | PPAnyInt
    | PPAny

instance Show PPattern where
    show (PPVar s pp) = s ++ " " ++ show pp
    show (PPString s) = show s
    show (PPRegex s) = ":/../"
    show PPAnyString = "$"
    show PPAnyInt = "#"
    show PPAny = "."

type PParser r = ParserT (LNToken String) Identity r

programp :: [String] -> PParser PExpr
programp eof = expressionp <* acceptL ["."] <* acceptL eof

expressionp :: PParser PExpr
expressionp = do e <- pipep
                 es <- many (acceptL [";"] *> expressionp)
                 return $ PBlock (e:es)

pipep :: PParser PExpr
pipep = do e <- firstp
           es <- many (acceptL ["|"] *> firstp)
           return $ foldl PPipe e es

operatorp :: [String] -> PParser PExpr -> PParser PExpr
operatorp ops subp = do e <- subp
                        es <- many (do op <- oneOfL ops
                                       e' <- subp
                                       return (content op, e'))
                        return $ foldl (\a (op, b) -> PCall op [a, b]) e es

firstp = concatp

concatp :: PParser PExpr
concatp = operatorp [".."] termp

termp :: PParser PExpr
termp = operatorp ["+", "-"] factorp

factorp :: PParser PExpr
factorp = operatorp ["*", "/"] simplep

simplep :: PParser PExpr
simplep = callp <|> forp <|> pullp <|> letp <|> intp <|> identp <|> parentp <|> stringp <|> objectp <|> emptyp <|> lenp

identp :: PParser PExpr
identp = PIdent <$> identifierL

stringp :: PParser PExpr
stringp = do s <- nextToken
             unless ((content s !! 0) == '\"') $
                 parsingError s "string token"
             return . PString $ drop 1 (content s)

intp :: PParser PExpr
intp = do s <- nextToken
          unless (all (`elem` "0123456789") (content s)) $
              parsingError s "int token"
          return . PInteger . read $ content s

objectp :: PParser PExpr
objectp = do acceptL ["<"]
             tag <- identifierL
             keyvals <- M.fromList <$> many keyvalp
             vals <- (acceptL ["/>"] *> pure [])
                     <|> (acceptL [">"] *> many simplep <* (acceptL ["</>"] <|> (acceptL ["</"] *> acceptL [tag] *> acceptL [">"])))
             return $ PObject tag keyvals vals

keyvalp :: PParser (String, PExpr)
keyvalp = (,) <$> identifierL <*> (acceptL ["="] *> simplep)

lenp :: PParser PExpr
lenp = PCall "#" . pure <$> (acceptL ["#"] *> simplep)

callp :: PParser PExpr
callp = PCall <$> identifierL <*> (acceptL ["("] *> argumentsp) <* acceptL [")"]

argumentsp :: PParser [PExpr]
argumentsp = (do a <- expressionp
                 as <- many (acceptL [","] *> expressionp)
                 return (a:as))
           <|> nothing

parentp :: PParser PExpr
parentp = (acceptL ["("] *> expressionp) <* acceptL [")"]

emptyp :: PParser PExpr
emptyp = acceptL ["(", ")"] >> return PEmpty

forp :: PParser PExpr
forp = PFor <$> some ((,) <$> patternp <*> (acceptL ["=>"] *> concatp))

pullp :: PParser PExpr
pullp = PPull <$> some ((,) <$> patternp <*> (acceptL ["->"] *> concatp))

letp :: PParser PExpr
letp = PLet <$> patternp <*> (acceptL ["="] *> pipep) <*> (acceptL [";"] *> expressionp)

patternp :: PParser PPattern
patternp = varpp <|> stringpp <|> anystringpp <|> regexpp <|> anyintpp <|> parentpp <|> anypp

varpp :: PParser PPattern
varpp = PPVar <$> identifierL <*> (patternp <|> return PPAny)

stringpp :: PParser PPattern
stringpp = do (PString s) <- stringp
              return $ PPString s

anystringpp :: PParser PPattern
anystringpp = acceptL ["$"] >> return PPAnyString

regexpp :: PParser PPattern
regexpp = do s <- nextToken
             unless ((content s !! 0) == '/') $
                 parsingError s "regex token"
             return . PPRegex . mkRegex $ drop 1 (content s)

anyintpp :: PParser PPattern
anyintpp = acceptL ["#"] >> return PPAnyInt

anypp :: PParser PPattern
anypp = acceptL ["."] >> return PPAny

parentpp :: PParser PPattern
parentpp = (acceptL ["("] *> patternp) <* acceptL [")"]

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

data PunaValue
    = PVStr String
    | PVInt Integer
    | PVObj String (M.Map String PunaValue) [PunaValue]

toString :: PunaValue -> String
toString (PVStr s) = s
toString (PVInt i) = show i
toString (PVObj t m l) = showObj t m l toString

toInt :: PunaValue -> Integer
toInt (PVStr s) = read s
toInt (PVInt i) = i
toInt (PVObj _ _ _) = error "can't convert object to integer"
type PunaScope = M.Map String PunaValue

type PunaFunc = PunaScope -> StreamT PunaValue IO ()

eval :: PExpr -> PunaScope -> (PunaValue -> StreamT PunaValue IO ()) -> StreamT PunaValue IO ()
eval expr scope g = pipePipe [compile expr scope, pullPipe >>= \v -> case v of Just v' -> g v'
                                                                               Nothing -> error $ "stream is empty: " ++ show expr ++ " evaluated to nothing" ]

expectOne :: [PunaValue] -> PunaValue
expectOne [x] = x
expectOne []  = error "stream is empty"
expectOne _   = error "stream is full"

compile :: PExpr -> PunaFunc
compile PEmpty           scope = return ()
compile (PString string) scope = pushPipe $ PVStr string
compile (PInteger int)   scope = pushPipe $ PVInt int
compile (PObject t m l)  scope = do m' <- lift . lift $ fmap expectOne <$> mapM (pullAllPipe . flip compile scope) m
                                    l' <- lift . lift $ concat <$> mapM (pullAllPipe . flip compile scope) l
                                    pushPipe $ PVObj t m' l'
compile (PIdent ident)   scope = pushPipe . fromJust $ M.lookup ident scope <|> Just (PVStr "")
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
    = eval val scope $ \v ->
      let (Just is) = patternToInserts pattern v
      in compile expr (foldr ($) scope is)

compile (PBlock exprs) scope
    = let (v:vs) = ($ scope) <$> map compile exprs
      in foldl (>>) v vs

compile (PCall ".." [a, b]) scope = binaryPvOp scope a b toString PVStr (++)
compile (PCall "+" [a, b])  scope = binaryPvOp scope a b toInt PVInt (+)
compile (PCall "-" [a, b])  scope = binaryPvOp scope a b toInt PVInt (-)
compile (PCall "*" [a, b])  scope = binaryPvOp scope a b toInt PVInt (*)
compile (PCall "/" [a, b])  scope = binaryPvOp scope a b toInt PVInt div
compile (PCall "#" [a])     scope = eval a scope $ \x -> pushPipe . PVInt . toInteger . length . toString $ x
compile (PCall f as)        scope = error ("unknown function " ++ f)

binaryOp :: PunaScope -> PExpr -> PExpr -> (PunaValue -> PunaValue -> StreamT PunaValue IO ()) -> StreamT PunaValue IO ()
binaryOp scope a b f = eval a scope $ \x ->
                       eval b scope $ \y ->
                       f x y

binaryPvOp :: PunaScope -> PExpr -> PExpr -> (PunaValue -> t) -> (t -> PunaValue) -> (t -> t -> t) -> StreamT PunaValue IO ()
binaryPvOp scope a b t c f = binaryOp scope a b $ \x y ->
                             let x' = t x
                                 y' = t y
                             in pushPipe . c $ f x' y'

-- Pattern

patternToInserts :: PPattern -> PunaValue -> Maybe [PunaScope -> PunaScope]
patternToInserts = pti'

pti' :: PPattern -> PunaValue -> Maybe [PunaScope -> PunaScope]

-- ppvar
pti' (PPVar v p) e = do inserts <- pti' p e
                        return (M.insert v e : inserts)

-- ppstring
pti' (PPString s) (PVStr e) = if s == e then Just [] else Nothing
pti' (PPString _) _         = Nothing

-- ppanystring
pti' PPAnyString (PVStr _) = Just []
pti' PPAnyString _         = Nothing

-- ppregex
pti' (PPRegex p) (PVStr e) = do groups <- matchRegex p e
                                return . map (\(i, g) -> M.insert ('\'':show i) (PVStr g)) $ zip [(1::Int)..] groups
pti' (PPRegex _) _         = Nothing

-- ppanyint
pti' PPAnyInt (PVInt _) = Just []
pti' PPAnyInt _         = Nothing

-- ppany
pti' PPAny _ = Just []

-- Runtime

inputStream :: CoroutineT PunaValue IO ()
inputStream = do done <- lift isEOF
                 unless done $ do
                     line <- lift getLine
                     push $ PVStr line
                     inputStream

outputStream :: CoroutineT PunaValue IO () -> IO ()
outputStream input = do p <- pull input
                        case p of
                            Just (s, l) -> do putStrLn $ toString s
                                              outputStream l
                            Nothing -> return ()

runProgram :: String -> IO ()
runProgram code = case program of
                      Left error -> forM_ error $ \msg -> print msg
                      Right io -> io
    where
        program = do expr <- parsePuna code
                     let func = compile expr
                     let scope = M.empty
                     return $ outputStream (() <$ pipe (NoCR <$ inputStream) (() <$ func scope))
