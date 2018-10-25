module Parser
       ( dataTypeName
       , dataTypeParser
       , defaultParseMode'
       , firstConstructor
       , justType
       , newTypeParser
       , parseDataDeclarations
       , parseNewTypes
       , recordDTParser
       , removeNewLines
       , singleProductDTParser
       , strictAndParentheses
       , strictOnly
       , sumDTParser
       , unParseDataDec
       , parseBeforePipe
       , equals

       , dataTypeParser'
       , dataTypeParserConstructorsWithSubTypes
       , finalDataTypeParser

       , normalDatatypeParser
       , unParseNormalDataDec
       , finalNormalDataTypeParser
       , newTypeParserFinal
       , newTypeUnparse
       , finalNewTypeParser
       ) where

import           Data.List                       (intercalate, intersperse,
                                                  unwords)
import           Language.Haskell.Exts.Extension (Extension (..), Language (..))
import           Language.Haskell.Exts.Fixity    (preludeFixities)
import           Language.Haskell.Exts.Parser    (ParseMode (..))
import           Text.Parsec                     (ParseError, Parsec, alphaNum,
                                                  anyChar, between, char, endBy,
                                                  eof, many, manyTill, newline,
                                                  noneOf, parse, sepBy, sepBy1,
                                                  sepEndBy, skipMany, space,
                                                  string, try, (<|>))

-- | General parsers

-- | Removes all newline characters.
removeNewLines :: [String] -> [String]
removeNewLines [] = []
removeNewLines (x:xs) = do
    case parse (skipMany newline *> try (many $ noneOf ['\n'] <* skipMany newline)) "" x of
        Left parseError -> error $ show parseError
        Right str       -> [str] ++ removeNewLines xs

-- | Data type parsers

-- | Parses data type definitions. Parsers in this package are
-- not interested in data constructors, only types existing in
-- other types.
-- TODO: Create a test for data type parsers via hedgehog
-- Prob need generators to create sum types and product types.
parseDataDeclarations :: [String] -> [(String, [String])]
parseDataDeclarations [] = []
parseDataDeclarations (x:xs) =
    case parse (try dataTypeParser <|> sumDTParser) "" x of
        Left _       -> parseDataDeclarations xs
        Right result -> [(head $ words result, tail $ words result)] ++ parseDataDeclarations xs


dataTypeName :: Parsec String () String
dataTypeName = string "data" *> many space *> many alphaNum <* many space

dataTypeParser :: Parsec String () String
dataTypeParser =  ((try singleProductDTParser) <|> recordDTParser)

firstConstructor :: Parsec String () String
firstConstructor = char '=' *> many space *> many alphaNum <* many space

singleProductDTParser :: Parsec String () String
singleProductDTParser = do
    typeName <- dataTypeName
    _ <- firstConstructor
    first <- firstParse
    rest <- secondParse
    pure $ typeName ++ " " ++ concat (intersperse " " ([first] ++ rest))
  where
        firstParse = char '{' *> many space *> recordAccessorTypeParse
        recordAccessorTypeParse = try (char '_')
                                      *> (many alphaNum)
                                      *> many (char ':') `sepBy` space
                                      *> ((try strictAndParentheses) <|> strictOnly)
        secondParse = many space *> manyTill (char ',' *> many space *> recordAccessorTypeParse) (many space *> char '}')

strictAndParentheses :: Parsec String () String
strictAndParentheses = char '!'
                    *> char '('
                    *> (concat <$> (sepBy1 (many alphaNum) (char ' '))) <* char ')'

strictOnly :: Parsec String () String
strictOnly = char '!' *> many alphaNum <* many space

justType :: Parsec String () String
justType = many alphaNum <* many space

sumDTParser :: Parsec String () String
sumDTParser = do
    typeName <- dataTypeName
    firstTypeOfFirstConst <- char '=' *> many space *> many alphaNum *> many space *> many alphaNum <* many space
    secondConstWithType <- concat <$> manyTill (char '|' *> many space *> (many alphaNum *> many space *> many alphaNum) `sepBy` (char ' ')) eof
    pure $ typeName ++ " " ++ concat (intersperse " " ([firstTypeOfFirstConst] ++ secondConstWithType))


recordDTParser :: Parsec String () String
recordDTParser = do
    typeName <- dataTypeName
    _ <- firstConstructor
    first <- firstParse
    rest <- restRecordAccessorType
    pure $ typeName  ++ " " ++ concat (intersperse " " ([first] ++ rest))
  where
    firstParse = char '{' *> many space *> firstRecordAccessorType
    firstRecordAccessorType =        many space
                                  *> many alphaNum *> many (char ':') `sepBy` space
                                  *> ((try strictAndParentheses) <|> strictOnly <|> justType)
    restRecordAccessorType = many space *> manyTill (char ',' *> many space *> firstRecordAccessorType) (many space *> char '}')

-- | Newtype parsers

-- | Parses newtype definitions.
parseNewTypes :: [String] -> [(String,[String])]
parseNewTypes [] = []
parseNewTypes (x:xs) =
    case parse newTypeParser "" x of
        Left _ -> parseNewTypes xs
        --TODO: You need a "super" parser to switch between "sub parsers" (type,data,newtype etc) so you can get your errors
                         -- (Data constructor   ,   type constructors)
        Right result    -> [(head $ words result, tail $ words result)] ++ parseNewTypes xs

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

-------------- MISC ----------------

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


-- RESTART --

-- data MyType = Nullary |
-- data MyType = Product One Two Three |
equals :: Parsec String () String
equals = string "=" *> many space

--                                 DataDecName, Constructors, Types
parseBeforePipe :: Parsec String () (String, [[String]])
parseBeforePipe = do
    dTName <- dataTypeName
    _ <- equals
    -- Gets constructors and potentially type constructors
    -- before pipe
    constructors <- (many alphaNum `sepBy` space) `sepBy` string "| "
    pure (dTName, constructors)


------------ NEW PARSERS ------------



-- you currently parse datatypes and newtypes
-- TODO: record accessors

-- DATA DECLARATION PARSERS

finalDataTypeParser :: String -> Either ParseError (String, [String])
finalDataTypeParser = parse dataTypeParser' ""

-- Nullary constructors e.g "data GFJiCsrQFUE = XDrQdCYeQJ | YYNL | TAQRMKZrL | DtRQA"
-- parse dataTypeParser' "" "data GFJiCsrQFUE = XDrQdCYeQJ | YYNL | TAQRMKZrL | DtRQA"
dataTypeParser' :: Parsec String () (String, [String])
dataTypeParser' = do
    _ <- string "data"
    _ <- many space
    typeName <- many alphaNum
    constructors <- many space *> string "=" *> many space *> dataTypeParserConstructors'
    pure (typeName, constructors)


dataTypeParserConstructors' :: Parsec String () [String]
dataTypeParserConstructors' = many alphaNum `sepBy` string " | "

unParseDataDec :: (String, [String]) -> String
unParseDataDec (dataDec, subTypes) =
    concat [ "data "
           , dataDec
           , " = "
           , concat $ intersperse " | " subTypes
           ]

-- Normal data declarations e.g "data QURiEn = BTyFRzH NFQV QsdDkPsMYr RbnTeFgTuOc ArdDbEcGrth
                                          -- | EpPAhQsZvi OkvBGcOmUsA
                                          -- | VgUKMNd
                                          -- | CZiZe IuPgaw CxRBJlu"


finalNormalDataTypeParser :: String -> Either ParseError (String, [[String]])
finalNormalDataTypeParser = parse normalDatatypeParser ""

normalDatatypeParser :: Parsec String () (String, [[String]])
normalDatatypeParser = do
    _ <- string "data"
    _ <- many space
    typeName <- many alphaNum
    constructors <- many space *> string "=" *> dataTypeParserConstructorsWithSubTypes
    pure (typeName, constructors)


dataTypeParserConstructorsWithSubTypes :: Parsec String () [[String]]
dataTypeParserConstructorsWithSubTypes = space *> constructorParser `sepBy` string "| "
  where
    constructorParser :: Parsec String () [String]
    constructorParser = many alphaNum `sepEndBy` space

unParseNormalDataDec :: (String, [[String]]) -> String
unParseNormalDataDec (dataDec, subTypes) =
    concat [ "data "
           , dataDec
           , " = "
           , intercalate " | " $ map (unwords . filter (/= "")) subTypes
           ]

-- Newtype declarations e.g "newtype QURiEn = QURiEn Gafalkm"

finalNewTypeParser :: String -> Either ParseError (String, String)
finalNewTypeParser = parse newTypeParserFinal ""

newTypeParserFinal :: Parsec String () (String, String)
newTypeParserFinal = do
    _ <- string "newtype"
    _ <- many space
    newTypeName <- many alphaNum
    _ <- string " = "
    _ <- many alphaNum
    _ <- many space
    wrappedType <- many alphaNum
    pure (newTypeName,wrappedType)

newTypeUnparse :: (String, String) -> String
newTypeUnparse tuple = concat [ "newtype "
                              , fst tuple
                              , " = "
                              , fst tuple
                              , " "
                              , snd tuple
                              ]

-- Record Accessor Parsers
-- TODO:
{-
recordAccessorParser :: Parsec String () (String, [(String, (String,String))])
recordAccessorParser = do
    _ <- string "data"
    _ <- many space
    typeName <- many alphaNum
    between (char '{'}) (char '{') (betweenParser)
    pure (typeName, )

-}