module Main where

--import           Conduit
--import qualified Data.Text                        as T
--import           System.FilePath                  (takeExtension)
import           Data.Either                  (isRight)
import           Language.Haskell.Exts.Parser (fromParseResult,
                                               parseModuleWithMode)
import           SrcManipulation              (dataDecHasRecordAccessor,
                                               getDataDecls, getNewTypeDecls,
                                               printDeclarations,
                                               returnListDecl)

import           Parser                       (defaultParseMode',
                                               finalNewTypeParser,
                                               mixedDatatypeParser',
                                               removeNewLines)
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

    -- Get [Decl SrcSpanInfo]
    let dataList = getDataDecls $ returnListDecl moduleSrsSpan
    let newTypeList = getNewTypeDecls $ returnListDecl moduleSrsSpan
    let recAccDataList = filter (isRight . dataDecHasRecordAccessor) (returnListDecl moduleSrsSpan)

    let dataRecAcc = removeNewLines $ printDeclarations recAccDataList
    let dataStrings = removeNewLines $ printDeclarations dataList
    let ntStrings = removeNewLines $ printDeclarations newTypeList

    print "Record Accessor Datatypes"
    print dataRecAcc

    print "Normal data declaration types"
    print dataStrings

    print "Newtype data declaration types"
    print ntStrings
    -- TODO: Newtypes can have record accessors and ' can be used in a datatype declaration.
    -- Need a parser that switches to appropriate parser based on
    print $ map mixedDatatypeParser' dataRecAcc
    print $ map mixedDatatypeParser' dataStrings
    print $ map finalNewTypeParser ntStrings
