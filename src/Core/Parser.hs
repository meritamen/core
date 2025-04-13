{-# LANGUAGE OverloadedStrings #-}

module Core.Parser (parseCore, parseCoreFile) where

import           Control.Exception              (throw)
import           Control.Monad                  (void)
import           Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import qualified Data.Text.IO                   as TIO
import           Text.Megaparsec
    ( choice
    , eof
    , many
    , notFollowedBy
    , runParser
    , sepBy
    , sepBy1
    , some
    , try
    , (<|>)
    )
import           Text.Megaparsec.Char           (space)

import Core.Language
import Core.Scanner

variable :: Parser Text
variable = choice $ [try alphaVariable]
  where alphaVariable = do
          v <- identifier
          if v `elem` keywords
            then fail . T.unpack $ "cannot use keyword " <> v <> " as variableiable"
            else return v

varP :: Parser CoreExpr
varP = EVar <$> variable

numP :: Parser CoreExpr
numP = ENum <$> integer

constrP :: Parser CoreExpr
constrP = do
  void $ symbol "Pack"
  braces $ do
    tag <- integer
    comma
    arity <- integer
    return $ EConstr tag arity

aexprP :: Parser CoreExpr
aexprP = varP <|> numP <|> constrP <|> parens exprP

caseP :: Parser CoreExpr
caseP = do
  void $ symbol "case"
  scrutinee <- exprP
  void $ symbol "of"
  alts <- caseAltP
  return $ ECase scrutinee alts

caseAltP :: Parser [CoreAlt]
caseAltP = altP `sepBy` try (semicolon <* notFollowedBy identifier)
  where
    altP = do
      tag <- angles integer
      args <- many variable
      rightArrow
      expr <- exprP
      return (tag, args, expr)

letP :: Parser CoreExpr
letP = do
  isRec <- isRecP
  defns <- letDefnP `sepBy1` semicolon
  void $ symbol "in"
  expr <- exprP
  return $ ELet isRec defns expr
  where
    isRecP = (symbol "letrec" >> return True)
            <|> (symbol "let" >> return False)
    letDefnP = do
      var <- variable
      equal
      expr <- exprP
      return (var, expr)

lamP :: Parser CoreExpr
lamP = do
  backSlash
  args <- many variable
  dot
  expr <- exprP
  return $ ELam args expr

operatorTable :: [[Operator Parser CoreExpr]]
operatorTable = (fmap . fmap) binary
                [ ["*", "/"]
                , ["+", "-"]
                , ["==", "~=", ">=", "<=", "+", "-", "*", "/", ">", "<"]
                , ["&", "|"]]
  where
    binary name = InfixL (mkBinaryAp name <$ symbol name)
    mkBinaryAp op = \l r -> EAp (EAp (EVar op) l) r

apP :: Parser CoreExpr
apP = makeExprParser (foldl1 EAp <$> some aexprP) operatorTable

exprP :: Parser CoreExpr
exprP = apP <|> letP <|> caseP <|> lamP <|> aexprP

scDefnP :: Parser CoreScDefn
scDefnP = do
  name <- variable
  args <- many variable
  equal
  body <- exprP
  return (name, args, body)

programP :: Parser CoreProgram
programP = space *> scDefnP `sepBy1` semicolon <* eof

parseCore :: Text -> CoreProgram
parseCore input = case runParser programP "<stdin>" input of
  Left err   -> throw err
  Right prog -> prog

parseCoreFile :: FilePath -> IO CoreProgram
parseCoreFile f = do
  contents <- TIO.readFile f
  case runParser programP f contents of
    Left err   -> throw err
    Right prog -> return prog
