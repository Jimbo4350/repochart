module SrcManipulation
    ( getDataDecls
    , getNewTypeDecls
    , printDeclarations
    , returnListDecl
    ) where

import           Language.Haskell.Exts.ExactPrint (exactPrint)
import           Language.Haskell.Exts.SrcLoc     (SrcSpanInfo (..))
import           Language.Haskell.Exts.Syntax

-- DataDecl l (DataOrNew l) (Maybe (Context l)) (DeclHead l) [QualConDecl l] [Deriving l]

returnListDecl :: Module SrcSpanInfo -> [Decl SrcSpanInfo]
returnListDecl (Module _ _ _ _ declList) = declList
returnListDecl _ = error "Error with returnListDecl"

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

-- | Uses ExactPrint instance to print the AST as it was parsed.
printDeclarations :: [Decl SrcSpanInfo] -> [String]
printDeclarations = map (flip exactPrint [])

