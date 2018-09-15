module Parser
       ( dataTypeName
       , dataTypeParser
       , firstConstructor
       , justType
       , recordDTParser
       , singleProductDTParser
       , strictAndParentheses
       , strictOnly
       , sumDTParser
       ) where

import Data.List (intersperse)
import           Text.Parsec (Parsec, alphaNum, char, many, manyTill, sepBy,
                              space, try, (<|>), eof, sepBy1, string)

-- | Data type parsers

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
