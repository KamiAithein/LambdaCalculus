module Parser
    (
        parse
    ) where

import Model
import Data.Text ( unpack, splitOn )
import Data.String ( IsString(fromString) )

data Token = Token Char | EOF

type ParseResult a = (a, [Char])

reservedSymbols :: [Char]
reservedSymbols = ['\\', '(', ')', '.']

{-
(\a.b)
\a.b
(\abc.abc) === (\a.(\b.(\c.abc)))
[x + y] <- (\ab.axby)  => c+d === (\ab.acbd) 
-}

splitOnString :: [Char] -> [Char] -> [[Char]]
splitOnString delim str = 
    let splits = splitOn (fromString delim) (fromString str)
    in map unpack splits

parseVar :: Char -> Variable Char
parseVar = Variable

parseParam :: [Char] -> Param Char
parseParam [var] = parseVar var
parseParam _ = undefined

parseBody :: [Char] -> ParseResult (Body Char)
parseBody = parseExpr

parseFunc :: [Char] -> ParseResult (Expr Char)
parseFunc xx@('\\':xs) = 
    let (params,dot:rest)  = break ('.'==) xs
        param          = parseParam params
        (body, rest')  = parseBody rest

    in (Exprf $ Function param body, rest')

parseFunc _ = undefined

{-
    (ab)        == (a(b))
    a(b)        == (a(b))
    a(b)c       == (a((b)c))
    a(bc)       == 
    a(b(c)d)e
-}

parseExpr :: [Char] -> ParseResult (Expr Char)
{- for now we ignore left parens -}
parseExpr ('(':rest) = parseExpr rest

parseExpr (a:rest) | a `notElem` reservedSymbols =
    let v = Exprv $ parseVar a
    in case rest of
        (')':rr) -> (v, rr)
        []       -> (v, [])
        _        -> let (e, rest') = parseExpr rest 
                    in (Expre v e, rest')

parseExpr xx@('\\':xs) = 
    let ff@(f, rest)  = parseFunc xx
    in case rest of 
        (')':rr) -> (f, rr)
        []       -> ff 
        _        -> let (e, rest') = parseExpr rest 
                    in (Expre f e, rest')

parseExpr _ = undefined

parse :: [Char] -> Expr Char
parse xx = acc xx []
    where
        acc :: [Char] -> [Expr Char] -> Expr Char 
        acc [] exprs = combine $ reverse exprs
        acc xx exprs = 
            let (expr, rest) = parseExpr xx
            in acc rest (expr:exprs)

        combine :: [Expr Char] -> Expr Char
        combine [x]    = x
        combine (x:xs) = Expre x (combine xs) 
        combine _ = undefined