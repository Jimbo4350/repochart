module Test.Gen
       ( genConstructorWithSubtypes
       , genDataDeclation
       , genDeclarationName
       , genNewTypeDeclaration
       , genNullaryDataDeclation
       , genConstructor
       , genRecord
       , genRecordAccessorConstructor
       ) where
import           Data.List      (intercalate, intersperse)
import           Hedgehog       (Gen)
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

genDeclarationName :: Gen String
genDeclarationName = do
    dataDecBody <- Gen.string (Range.constant 3 10) Gen.alpha
    capital <- Gen.string (Range.singleton 1) Gen.upper
    pure $ capital ++ dataDecBody

genConstructorWithSubtypes :: Gen String
genConstructorWithSubtypes = do
    constructs <- Gen.list (Range.constant 1 5) genDeclarationName
    pure . concat $ intersperse " " constructs

genNullaryDataDeclation :: Gen String
genNullaryDataDeclation = do
    dta <- Gen.constant "data "
    dName <- genDeclarationName
    nullaryConstructors <- Gen.list (Range.constant 1 5) genDeclarationName
    pipe <- Gen.constant " | "
    equal <- Gen.constant " = "
    let allConstructors = intersperse pipe nullaryConstructors
    pure . concat $ [dta, dName, equal] ++ allConstructors

genDataDeclation :: Gen String
genDataDeclation = do
    dta <- Gen.constant "data "
    dName <- genDeclarationName
    constructs <- Gen.list (Range.constant 1 5) genConstructorWithSubtypes
    pipe <- Gen.constant " | "
    equal <- Gen.constant " = "
    let allConstructors = intersperse pipe constructs
    pure . concat $ [dta, dName, equal] ++ allConstructors

genNewTypeDeclaration :: Gen String
genNewTypeDeclaration = do
    ntype <- Gen.constant "newtype "
    nTName <- genDeclarationName
    equal <- Gen.constant " = "
    wrappedType <- genDeclarationName
    pure $ concat [ntype, nTName, equal, nTName, " ", wrappedType]

-- These generators generate strings for the sub parsers
-- of the record accessor parser.

genConstructor :: Gen String
genConstructor = do
    constDecBody <- Gen.string (Range.constant 3 10) Gen.alpha
    capital <- Gen.string (Range.singleton 1) Gen.upper
    pure (capital ++ constDecBody)

genRecord :: Gen String
genRecord = do
    record <- Gen.string (Range.constant 3 10) Gen.lower
    doubleColon <- Gen.constant " :: "
    typeConst <- genConstructor
    pure $ concat [record,doubleColon,typeConst]

genRecordAccessorConstructor :: Gen String
genRecordAccessorConstructor = do
    dta <- Gen.constant "data "
    dName <- genDeclarationName
    equal <- Gen.constant " = "
    const <- genConstructor
    records <- Gen.list (Range.constant 2 5) genRecord
    pure $ concat [dta, dName, equal, const, " { ", intercalate " , " records, " }"]
