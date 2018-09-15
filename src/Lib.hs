module Lib
    ( defaultParseMode'
    , newTypeParser
    , normalNT
    , printDeclarations
    , parseAllDataDecls
    , parseDataTypes
    , parseNewTypes
    , recordNT
    , returnListDecl
    ) where

import           Language.Haskell.Exts.ExactPrint (exactPrint)
import           Language.Haskell.Exts.Extension  (Extension (..),
                                                   Language (..))
import           Language.Haskell.Exts.Fixity     (preludeFixities)
import           Language.Haskell.Exts.Parser     (ParseMode (..))
import           Language.Haskell.Exts.SrcLoc     (SrcSpanInfo (..))
import           Language.Haskell.Exts.Syntax
import           Parser                           (dataTypeParser, sumDTParser)
import           Text.Parsec

-- DataDecl l (DataOrNew l) (Maybe (Context l)) (DeclHead l) [QualConDecl l] [Deriving l]

returnListDecl :: Module SrcSpanInfo -> [Decl SrcSpanInfo]
returnListDecl (Module _ _ _ _ declList) = declList
returnListDecl _ = error "Error with returnListDecl"

-- | Uses ExactPrint instance to print the AST as it was parsed.
printDeclarations :: [Decl SrcSpanInfo] -> [String]
printDeclarations declList = map (flip exactPrint []) declList

-- | Removes all newline characters.
parseAllDataDecls :: [String] -> [String]
parseAllDataDecls [] = []
parseAllDataDecls (x:xs) = do
    case parse (skipMany newline *> try (many $ noneOf ['\n'] <* skipMany newline)) "" x of
        Left parseError -> error $ show parseError
        Right str       -> [str] ++ parseAllDataDecls xs

-- | Parses data type definitions.
parseDataTypes :: [String] -> [(String, [String])]
parseDataTypes [] = []
parseDataTypes (x:xs) =
    --case parse (string "data" *> space *> many anyChar) "" x of
    case parse (try dataTypeParser <|> sumDTParser) "" x of
        Left _ -> parseDataTypes xs
        Right result       -> [(head $ words result, tail $ words result)] ++ parseDataTypes xs

-- | Parses newtype definitions.
parseNewTypes :: [String] -> [(String,[String])]
parseNewTypes [] = []
parseNewTypes (x:xs) =
    case parse newTypeParser "" x of
        Left _ -> parseNewTypes xs
        --TODO: You need a "super" parser to switch between "sub parsers" (type,data,newtype etc) so you can get your errors
                         -- (Data constructor   ,   type constructors)
        Right result    -> [(head $ words result, tail $ words result)] ++ parseNewTypes xs

-- | Newtype parsers

newTypeParser :: Parsec String () String
newTypeParser =
    string "newtype" *> (many alphaNum) `sepBy` space *> char '=' *> ((try recordNT) <|> derivingNT <|> normalNT)

derivingNT :: Parsec String () String
derivingNT = manyTill anyChar (string "deriving")

normalNT :: Parsec String () String
normalNT = many space *> many anyChar

recordNT:: Parsec String () String
recordNT = do
    ntName <- many space *> many alphaNum <* many space
    ntSubtype <- between
                     (char '{')
                     (char '}')
                     ((many alphaNum) `sepBy` space *> many (char ':') *> many space *> many alphaNum <* many space)
    pure $ ntName ++ " " ++ ntSubtype

defaultParseMode' :: ParseMode
defaultParseMode' = ParseMode {
        parseFilename = "<unknown>.hs",
        baseLanguage = Haskell2010,
        extensions = map EnableExtension [minBound .. maxBound],
        ignoreLanguagePragmas = True,
        ignoreLinePragmas = True,
        fixities = Just preludeFixities,
        ignoreFunctionArity = False
        }
