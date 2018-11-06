module Parser
       ( defaultParseMode'
       , removeNewLines
       , unParseDataDec
       , dataTypeParser'
       , dataTypeParserConstructorsWithSubTypes
       , finalDataTypeParser
       , normalDatatypeParser
       , unParseNormalDataDec
       , finalNormalDataTypeParser
       , newTypeParserFinal
       , newTypeUnparse
       , finalNewTypeParser
       -- Record Accessor parser
       , recordAccessorsParser
       , recordAccessorDataTypeParser
       , unparseRecord
       , parseRecord
       , unparseMixedDataType
       , parseMultipleRecords
       , unparseMultipleRecords
       , mixedDatatypeParser'
       , constructorsParse
       , normalSubtypeParse
       , unparseRecordsOrConstructors
       ) where

import           Data.Char                       (isSpace)
import           Data.List                       (dropWhileEnd, intercalate,
                                                  intersperse, replicate,
                                                  unwords, unzip, zipWith)
import           Language.Haskell.Exts.Extension (Extension (..), Language (..))
import           Language.Haskell.Exts.Fixity    (baseFixities, preludeFixities)
import           Language.Haskell.Exts.Parser    (ParseMode (..))
import           Text.Parsec                     (ParseError, Parsec, alphaNum,
                                                  anyChar, between, char, eof,
                                                  many, manyTill, newline,
                                                  noneOf, oneOf, parse, sepBy,
                                                  sepBy1, sepEndBy, skipMany, option,
                                                  space, string, try, (<|>))

-- | General parsers

-- | Removes all newline characters.
removeNewLines :: [String] -> [String]
removeNewLines [] = []
removeNewLines (x:xs) =
    case parse (skipMany newline *> try (many $ noneOf ['\n'] <* skipMany newline)) "" x of
        Left parseError -> error $ show parseError
        Right str       -> [str] ++ removeNewLines xs


-------------- MISC ----------------

defaultParseMode' :: ParseMode
defaultParseMode' = ParseMode {
        parseFilename = "<unknown>.hs",
        baseLanguage = Haskell2010,
        extensions = map EnableExtension [minBound .. maxBound],
        ignoreLanguagePragmas = True,
        ignoreLinePragmas = True,
        fixities = Just (preludeFixities ++ baseFixities),
        ignoreFunctionArity = False
        }

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
-- Note a record accessor can show up in any constructor of a
-- given sum type therefore you must first parse with record accessor
-- parser then a data constructor parser.
                                                 -- Type, Constructor, [record,type]
                                                 -- TODO: Feed this through the parsers,
                                                 -- generators and unparsers
                                                 -- This parser is what she be tested
                                                 -- in parseMultipleRecords
recordAccessorDataTypeParser :: Parsec String () (String, String, [(String, String)])
recordAccessorDataTypeParser = do
    _ <- string "data"
    _ <- many space
    typeName <- many alphaNum
    _ <- many space
    _ <- string "="
    _ <- many space
    consName <- many alphaNum
    _ <- many space
    accessorsAndConstructors <- recordAccessorsParser
    pure (typeName, consName, accessorsAndConstructors)


parseRecord :: String -> Either ParseError (String, String)
parseRecord = parse singleRecordAccParser ""

unparseRecord :: (String, String) -> String
unparseRecord (recAccessor, recType) = concat [recAccessor, " :: ", dropWhileEnd isSpace recType]

parseMultipleRecords :: String -> Either ParseError (String, String, [(String, String)])
parseMultipleRecords = parse recordAccessorDataTypeParser ""

unparseMultipleRecords :: (String, String, [(String, String)]) -> String
unparseMultipleRecords (typeName, constructorName, records) =
    "data " ++ typeName ++ " = " ++ constructorName ++ " " ++ "{ " ++ helper records
  where
    helper :: [(String, String)] -> String
    helper []       = []
    helper [x]      = unparseRecord x ++ " }"
    helper (x : xs) = unparseRecord x ++ " , " ++ helper xs


mixedDatatypeParser' :: String -> Either ParseError (String, [(String, [(String, String)])])
mixedDatatypeParser' = parse mixedDatatypeParser ""

mixedDatatypeParser :: Parsec String () (String, [(String, [(String, String)])])
mixedDatatypeParser = do
    _ <- string "data"
    _ <- many space
    typeName <- try (many alphaNum) <|> many (oneOf "'")
    _ <- many space
    _ <- string "="
    constructors <- constructorsParse `sepBy` string "|"
    pure (typeName, constructors)

constructorsParse :: Parsec String () (String, [(String, String)])
constructorsParse = do
    _ <- many space
    consName <- many alphaNum
    _ <- many space
    types <- try recordAccessorsParser <|> normalSubtypeParse
    _ <- many space
    pure (consName, types)

-- Parses one or multiple records
recordAccessorsParser :: Parsec String () [(String, String)]
recordAccessorsParser =
    between (char '{') (char '}') (singleRecordAccParser `sepBy` string ",")

singleRecordAccParser :: Parsec String () (String, String)
singleRecordAccParser = do
    _ <- many space
    recAcessor <- many alphaNum
    _ <- many space
    _ <- string "::"
    _ <- many space
    exclaimation <- option "" (string "!")
    openB <- option "" (string "(")
    recType <- many alphaNum `sepBy` (space <|> char '\'')
    closeB <- option "" (string ")")
    _ <- many space
    pure (recAcessor, exclaimation ++ openB ++ concat recType ++ closeB)

normalSubtypeParse :: Parsec String () [(String, String)]
normalSubtypeParse = do
    const <- (many alphaNum `sepBy` space)
    --subTypes <- option [""] (many alphaNum `sepEndBy` space)
    let filler = replicate (length const) ""
    --pure [("", const)]
    pure $ zip filler const

unparseMixedDataType :: (String, [(String, [(String, String)])]) -> String
unparseMixedDataType (typeName, constructors) = do
    let rest = intercalate " | " $ map unparseRecordsOrConstructors constructors
    concat ["data ", typeName, " = ", rest ]

unparseRecordsOrConstructors :: (String, [(String, String)]) -> String
unparseRecordsOrConstructors (_, []) = ""
unparseRecordsOrConstructors (const, [("", "")]) =
    const
unparseRecordsOrConstructors (const, ("", subType) : rest) = do
    let removeEmpties = filter (\x -> snd x /= "") rest
    dropWhileEnd isSpace $ const ++ " " ++ subType ++ " " ++ unwords (map snd removeEmpties)
unparseRecordsOrConstructors (const, recordTypes) = do
    let (records, assocTypes) = unzip recordTypes
    let zipRecords = zipWith (\ a b -> a ++ " :: " ++ dropWhileEnd isSpace b) records assocTypes
    let finalRecords = intercalate " , " zipRecords
    concat [const, " { ", finalRecords, " }"]

