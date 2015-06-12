{
module HParser where

import Types     (UQuery(..),UF(..))
import Data.Char (isSpace,isAlpha,isDigit)
import Data.List (isPrefixOf)
}

%name parse
%tokentype { Token }
%error { parseError }

%token
  var    { TokenVar $$ }
  int    { TokenInt $$ }
  string { TokenString $$ }
  bool   { TokenBool $$ }
  '{'    { TokenOB }
  '}'    { TokenCB }
  '|'    { TokenPipe }
  '<-'   { TokenBind }
  ','    { TokenComma }
  '('    { TokenOP }
  ')'    { TokenCP }
  '∈'    { TokenElementOf }
  '∪'    { TokenUnion }
  '∩'    { TokenIntersect }
  with   { TokenWith}

%%
Query :: { UQuery }
      : '{' App '|' CompExp '}' { UCompr $2 (reverse $4) }
      | App                     { $1 }
--      | With                    { $1 }

BindExp :: { UQuery }
        : var '<-' Query        { UBind' $1 $3 }
        
CompExp :: { [UQuery] }
        : BindExp               { [$1] }
        | App                   { [$1] }
        | CompExp ',' BindExp   { $3:$1 }
        | CompExp ',' Exp       { $3:$1 }
        
Exp   :: { UQuery }
      : Tuple                   { $1 }
--    | App                     { $1 }
      | int                     { UNumLit $1 }
      | string                  { UStringLit $1 }
      | var                     { URef $1 }
      | '(' Exp ')'             { $2 }

App   :: { UQuery }
--    : Exp Exp                 { UAppExpr $1 $2 }
      : App Exp                 { UAppExpr $1 $2 }
      | Exp                     { $1 }
--    | App '(' App ')'         { UAppExpr $1 $3 }
--    | var                     { URef $1 }
{-
With :: { UQuery }
     : with string Query        { UWith $2 $3 }
-}
Tuple :: { UQuery }        
Tuple : '(' TExps ')'           { UTuple (reverse $2) }

TExps :: { [UQuery] }
TExps : Exp ',' Exp             { [$3,$1] }
      | TExps ',' Exp           { $3:$1 }
      
{

data Token = TokenVar String
           | TokenInt Int
           | TokenString String
           | TokenBool Bool
           | TokenOB
           | TokenCB
           | TokenOP
           | TokenCP
           | TokenPipe
           | TokenBind
           | TokenComma
           | TokenWith
           | TokenElementOf
           | TokenUnion
           | TokenIntersect
           deriving Show

lexer :: String -> [Token]
lexer "" = []
lexer s
    | "<-" `isPrefixOf` s = TokenBind : lexer (drop 2 s)
    | "with" `isPrefixOf` s = TokenWith : lexer (drop 4 s)
lexer s@(c:cs)
      | isSpace c = lexer cs
      | isAlpha c = lexVar s
      | isDigit c = lexNum s
lexer ('{':cs) = TokenOB : lexer cs
lexer ('}':cs) = TokenCB : lexer cs
lexer ('|':cs) = TokenPipe : lexer cs
lexer ('"':cs) = lexString cs
lexer ('(':cs) = TokenOP : lexer cs
lexer (')':cs) = TokenCP : lexer cs
lexer (',':cs) = TokenComma : lexer cs

lexVar s = TokenVar v : lexer s'
    where (v,s') = span isAlpha s

lexNum s = TokenInt (read n) : lexer s'
    where (n,s') = span isDigit s

lexString s = TokenString lit : lexer (tail s')
    where (lit,s') = break (== '"') s
                               
parseError :: [Token] -> a
parseError _ = error "Parse error"

transform :: UQuery -> UQuery
transform (UCompr ret xs) = app
    where app  = foldr step ret' xs
          step (UBind' var m) acc = UBind m (UF var acc)
          step p              acc = UGuard p acc
          ret'                    = UReturn ret
transform query           = query
}
