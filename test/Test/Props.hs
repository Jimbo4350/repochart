{-# LANGUAGE TemplateHaskell #-}

module Test.Props
       ( tests
       ) where

import           Hedgehog.Internal.Property (Property, failWith, forAll,
                                             property, (===))
import           Hedgehog.Internal.Runner   (checkParallel)
import           Hedgehog.Internal.TH       (discover)

import           Parser                     (finalDataTypeParser,
                                             finalNewTypeParser,
                                             finalNormalDataTypeParser,
                                             mixedDatatypeParser',
                                             newTypeUnparse,
                                             normalDatatypeParser,
                                             parseMultipleRecords, parseRecord,
                                             unParseDataDec,
                                             unParseNormalDataDec,
                                             unparseMultipleRecords,
                                             unparseRecord, unparseMixedDataType)

import           Test.Gen                   (genDataDeclation, genMixedDatatype,
                                             genNewTypeDeclaration,
                                             genNonPartialRecordAccessorDataType,
                                             genNullaryDataDeclation, genRecord)

-- Potentially unnecessary as the parser tested in the last 2 properties
-- works for nullary constructor only datatypes
prop_parseUnparseOnlyNullaryConstructors :: Property
prop_parseUnparseOnlyNullaryConstructors = property $ do
    nulDataDec <- forAll genNullaryDataDeclation
    case finalDataTypeParser nulDataDec of
        Left err  -> failWith Nothing $ show err
        Right str -> unParseDataDec str === nulDataDec

prop_parseUnparseNormalDataDeclataions :: Property
prop_parseUnparseNormalDataDeclataions = property $ do
    dataDec <- forAll genDataDeclation
    case finalNormalDataTypeParser dataDec of
        Left err  -> failWith Nothing $ show err
        Right str -> unParseNormalDataDec str === dataDec

prop_parseUnparseNormalDataDeclataionsNullary :: Property
prop_parseUnparseNormalDataDeclataionsNullary = property $ do
    nulDataDec <- forAll genNullaryDataDeclation
    case finalNormalDataTypeParser nulDataDec of
        Left err  -> failWith Nothing $ show err
        Right str -> unParseNormalDataDec str === nulDataDec

prop_parseUnparseNewtype :: Property
prop_parseUnparseNewtype = property $ do
    newType <- forAll genNewTypeDeclaration
    case finalNewTypeParser newType of
        Left err  -> failWith Nothing $ show err
        Right str -> newTypeUnparse str === newType

-- Record accessor parsers tests

-- Only tests this string format:
-- auianfjlv :: RVETBtok
prop_parseUnparseSingleRecord :: Property
prop_parseUnparseSingleRecord = property $ do
    record <- forAll genRecord
    case parseRecord record of
        Left err  -> failWith Nothing $ show err
        Right str -> unparseRecord str === record

-- Only tests this string format:
-- data SomeType = SomeConstructor { auianfjlv :: RVETBtok , dcyqnvbgd :: DoYQ , eylexcij :: HQRUyN , hjmahiwegm :: GnvcHvVxJ }
prop_parseUnparseMultipleRecords :: Property
prop_parseUnparseMultipleRecords = property $ do
    records <- forAll genNonPartialRecordAccessorDataType
    case parseMultipleRecords records of
        Left err  -> failWith Nothing $ show err
        Right str -> unparseMultipleRecords str === records

-- Only tests this string format:
-- data SomeType = SomeConstructor { eylexcij :: HQRUyN , hjmahiwegm :: GnvcHvVxJ } | AnotherConstructor
prop_parseUnparseDataDecl :: Property
prop_parseUnparseDataDecl = property $ do
    records <- forAll genMixedDatatype
    case mixedDatatypeParser' records of
        Left err  -> failWith Nothing $ show err
        Right str -> unparseMixedDataType str === records

tests :: IO Bool
tests =
  checkParallel $$(discover)
