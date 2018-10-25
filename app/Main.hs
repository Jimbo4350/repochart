module Main where

--import           Conduit
--import qualified Data.Text                        as T
--import           System.FilePath                  (takeExtension)
import           Language.Haskell.Exts.Parser (fromParseResult,
                                               parseModuleWithMode)
import           SrcManipulation              (getDataDecls, printDeclarations,
                                               returnListDecl, getNewTypeDecls)

import           Parser                       (defaultParseMode',
                                               finalNormalDataTypeParser,
                                               parseDataDeclarations,
                                               removeNewLines,
                                               finalNewTypeParser)
{-
main :: IO ()
main =
    runConduitRes
     $ sourceFile "test/TestModule2.hs"
    .| decodeUtf8C
    .| mapC T.unpack
    .| mapC (parseModuleWithMode defaultParseMode')
    .| mapC fromParseResult
    .| mapC returnListDecl
    .| mapC printDeclarations
    .| mapC parseAllDataDecls
    .| mapC (\x -> parseDataTypes x ++ parseNewTypes x)
    .| mapC show
    .| mapC T.pack
    .| encodeUtf8C
    .| stdoutC
-}
{-
main :: IO ()
main =
    runConduitRes
     $ sourceDirectoryDeep True "."
    .| filterC (\fp -> takeExtension fp == ".hs")
    .| awaitForever sourceFile
    .| decodeUtf8C
    .| mapC T.unpack
    .| mapC (parseModuleWithMode defaultParseMode')
    .| mapC fromParseResult
    .| mapC returnListDecl
    .| mapC printDeclarations
    .| mapC parseAllDataDecls
    .| mapC (\x -> parseDataTypes x ++ parseNewTypes x)
    .| mapC show
    .| mapC T.pack
    .| encodeUtf8C
    .| stdoutC
-}

main :: IO ()
main = do
    contents <- readFile "test/TestModule.hs"
    let moduleSrsSpan = fromParseResult $ parseModuleWithMode defaultParseMode' contents
    let dataList = getDataDecls $ returnListDecl moduleSrsSpan
    let newTypeList = getNewTypeDecls $ returnListDecl moduleSrsSpan
    let dataStrings = map finalNormalDataTypeParser . removeNewLines $ printDeclarations dataList
    let ntStrings = map finalNewTypeParser . removeNewLines $ printDeclarations newTypeList
    print dataStrings
    print ntStrings
