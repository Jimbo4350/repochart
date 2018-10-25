module Test.Gen
       ( genConstructorWithSubtypes
       , genDataDeclation
       , genDeclarationName
       , genNewTypeDeclaration
       , genNullaryDataDeclation
       ) where
import           Data.List      (intersperse)
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