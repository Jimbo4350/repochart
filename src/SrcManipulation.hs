module SrcManipulation
    ( dataDecHasRecordAccessor
    , getDataDecls
    , getNewTypeDecls
    , printDeclarations
    , returnListDecl
    ) where

import           Language.Haskell.Exts.ExactPrint (exactPrint)
import           Language.Haskell.Exts.SrcLoc     (SrcSpanInfo (..))
import           Language.Haskell.Exts.Syntax

-- DataDecl l (DataOrNew l) (Maybe (Context l)) (DeclHead l) [QualConDecl l] [Deriving l]

-- You don't propagate this Left. Is is necessary?
dataDecHasRecordAccessor :: Decl SrcSpanInfo -> Either String (Decl SrcSpanInfo)
dataDecHasRecordAccessor dataDecl = do
    let qualConList = getQualConList dataDecl
    case True `elem` map recordChecker qualConList of
        True -> Right dataDecl
        False -> Left "dataDecHasRecordAccessor: No record accessor in this datatype"

recordChecker :: QualConDecl l -> Bool
recordChecker (QualConDecl _ _ _ RecDecl {}) = True
recordChecker _ = False

getDataDecls :: [Decl SrcSpanInfo] -> [Decl SrcSpanInfo]
getDataDecls [] = []
getDataDecls (DataDecl l (DataType dt) (Just context) declHead qualConList derivingList : xs)
    = DataDecl l (DataType dt) (Just context) declHead qualConList derivingList : getDataDecls xs
getDataDecls (DataDecl l (DataType dt) Nothing declHead qualConList derivingList :  xs)
    = DataDecl l (DataType dt) Nothing declHead qualConList derivingList : getDataDecls xs
getDataDecls (_ : xs) = getDataDecls xs

getNewTypeDecls :: [Decl SrcSpanInfo] -> [Decl SrcSpanInfo]
getNewTypeDecls [] = []
getNewTypeDecls (DataDecl l (NewType dt) (Just context) declHead qualConList derivingList : xs)
    = DataDecl l (NewType dt) (Just context) declHead qualConList derivingList : getNewTypeDecls xs
getNewTypeDecls (DataDecl l (NewType dt) Nothing declHead qualConList derivingList :  xs)
    = DataDecl l (NewType dt) Nothing declHead qualConList derivingList : getNewTypeDecls xs
getNewTypeDecls (_ : xs) = getNewTypeDecls xs

getQualConList :: Decl SrcSpanInfo -> [QualConDecl SrcSpanInfo]
getQualConList (DataDecl _ _ _ _ qualConDecList _ ) = qualConDecList
getQualConList _ = []

-- | Uses ExactPrint instance to print the AST as it was parsed.
printDeclarations :: [Decl SrcSpanInfo] -> [String]
printDeclarations = map (flip exactPrint [])

returnListDecl :: Module SrcSpanInfo -> [Decl SrcSpanInfo]
returnListDecl (Module _ _ _ _ declList) = declList
returnListDecl _ = error "Error with returnListDecl"


