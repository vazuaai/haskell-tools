{-# LANGUAGE TemplateHaskell
           , DataKinds
           , FlexibleInstances
           , MultiParamTypeClasses
           , FlexibleContexts
           , UndecidableInstances
           , TypeFamilies
           , ScopedTypeVariables
           #-}
module Language.Haskell.Tools.AST.Instances.ClassyPlate where

import Data.Generics.ClassyPlate
import Data.Generics.ClassyPlate.TH
import Data.Generics.ClassyPlate.Core

import Language.Haskell.Tools.AST.Representation.Modules
import Language.Haskell.Tools.AST.Representation.TH
import Language.Haskell.Tools.AST.Representation.Decls
import Language.Haskell.Tools.AST.Representation.Binds
import Language.Haskell.Tools.AST.Representation.Exprs
import Language.Haskell.Tools.AST.Representation.Stmts
import Language.Haskell.Tools.AST.Representation.Patterns
import Language.Haskell.Tools.AST.Representation.Types
import Language.Haskell.Tools.AST.Representation.Kinds
import Language.Haskell.Tools.AST.Representation.Literals
import Language.Haskell.Tools.AST.Representation.Names
import Language.Haskell.Tools.AST.Ann

import Language.Haskell.TH
import Control.Monad.IO.Class


-- this instance will invoke the app only once on a list
instance (GoodOperationFor c [a], ClassyPlate c a) => ClassyPlate c [a] where
  topDown_ t f ls = map (topDown_ t f) $ app (undefined :: FlagToken (AppSelector c [a])) t f ls
  topDownM_ t f ls = mapM (topDownM_ t f) =<< appM (undefined :: FlagToken (AppSelector c [a])) t f ls

  bottomUp_ t f ls = app (undefined :: FlagToken (AppSelector c [a])) t f $ map (bottomUp_ t f) ls
  bottomUpM_ t f ls = appM (undefined :: FlagToken (AppSelector c [a])) t f =<< mapM (bottomUpM_ t f) ls

  descend_ t f ls = map (appTD (undefined :: FlagToken (AppSelector c a)) t f (descend_ t f)) ls
  descendM_ t f ls = mapM (appTDM (undefined :: FlagToken (AppSelector c a)) t f (descendM_ t f)) ls

instance (GoodOperationFor c (Maybe a), ClassyPlate c a) => ClassyPlate c (Maybe a) where
  topDown_ t f mb = fmap (topDown_ t f) $ app (undefined :: FlagToken (AppSelector c (Maybe a))) t f mb
  topDownM_ t f mb = maybe (return Nothing) (fmap Just . topDownM_ t f) =<< appM (undefined :: FlagToken (AppSelector c (Maybe a))) t f mb

  bottomUp_ t f mb = app (undefined :: FlagToken (AppSelector c (Maybe a))) t f $ fmap (bottomUp_ t f) mb
  bottomUpM_ t f mb = appM (undefined :: FlagToken (AppSelector c (Maybe a))) t f =<< maybe (return Nothing) (fmap Just . bottomUpM_ t f) mb

  descend_ t f mb = fmap (appTD (undefined :: FlagToken (AppSelector c a)) t f (descend_ t f)) mb
  descendM_ t f mb = maybe (return Nothing) (fmap Just . appTDM (undefined :: FlagToken (AppSelector c a)) t f (descendM_ t f)) mb


-- Annotations
makeClassyPlate [ Right '_annotation ] ''Ann
makeClassyPlate [ Right '_annMaybeAnnot ] ''AnnMaybeG
makeClassyPlate [ Right '_annListAnnot ] ''AnnListG

-- Modules
makeClassyPlate [] ''UModule
makeClassyPlate [] ''UModuleHead
makeClassyPlate [] ''UExportSpecs
makeClassyPlate [] ''UExportSpec
makeClassyPlate [] ''UIESpec
makeClassyPlate [] ''USubSpec
makeClassyPlate [] ''UModulePragma
makeClassyPlate [] ''UFilePragma
makeClassyPlate [] ''UImportDecl
makeClassyPlate [] ''UImportSpec
makeClassyPlate [] ''UImportQualified
makeClassyPlate [] ''UImportSource
makeClassyPlate [] ''UImportSafe
makeClassyPlate [] ''UTypeNamespace
makeClassyPlate [] ''UImportRenaming

-- Declarations
makeClassyPlate [] ''UDecl
makeClassyPlate [] ''UClassBody
makeClassyPlate [] ''UClassElement
makeClassyPlate [] ''UDeclHead
makeClassyPlate [] ''UInstBody
makeClassyPlate [] ''UInstBodyDecl
makeClassyPlate [] ''UGadtConDecl
makeClassyPlate [] ''UGadtConType
makeClassyPlate [] ''UFieldWildcard
makeClassyPlate [] ''UFunDeps
makeClassyPlate [] ''UFunDep
makeClassyPlate [] ''UConDecl
makeClassyPlate [] ''UFieldDecl
makeClassyPlate [] ''UDeriving
makeClassyPlate [] ''UInstanceRule
makeClassyPlate [] ''UInstanceHead
makeClassyPlate [] ''UTypeEqn
makeClassyPlate [] ''UKindConstraint
makeClassyPlate [] ''UTyVar
makeClassyPlate [] ''UType
makeClassyPlate [] ''UKind
makeClassyPlate [] ''UContext
makeClassyPlate [] ''UAssertion
makeClassyPlate [] ''UExpr
makeClassyPlate [] ''UStmt'
makeClassyPlate [] ''UCompStmt
makeClassyPlate [] ''UValueBind
makeClassyPlate [] ''UPattern
makeClassyPlate [] ''UPatternField
makeClassyPlate [] ''USplice
makeClassyPlate [ Right '_qqString ] ''QQString
makeClassyPlate [] ''UMatch
makeClassyPlate [] ''UAlt'
makeClassyPlate [] ''URhs
makeClassyPlate [] ''UGuardedRhs
makeClassyPlate [] ''UFieldUpdate
makeClassyPlate [] ''UBracket
makeClassyPlate [] ''UTopLevelPragma
makeClassyPlate [] ''URule
makeClassyPlate [] ''UAnnotationSubject
makeClassyPlate [] ''UMinimalFormula
makeClassyPlate [] ''UExprPragma
makeClassyPlate [] ''USourceRange
makeClassyPlate [ Right '_numberInteger ] ''Number
makeClassyPlate [] ''UQuasiQuote
makeClassyPlate [] ''URhsGuard
makeClassyPlate [] ''ULocalBind
makeClassyPlate [] ''ULocalBinds
makeClassyPlate [] ''UFixitySignature
makeClassyPlate [] ''UTypeSignature
makeClassyPlate [] ''UListCompBody
makeClassyPlate [] ''UTupSecElem
makeClassyPlate [] ''UTypeFamily
makeClassyPlate [] ''UTypeFamilySpec
makeClassyPlate [] ''UInjectivityAnn
makeClassyPlate [] ''UCaseRhs'
makeClassyPlate [] ''UGuardedCaseRhs'
makeClassyPlate [] ''UPatternSynonym
makeClassyPlate [] ''UPatSynRhs
makeClassyPlate [] ''UPatSynLhs
makeClassyPlate [] ''UPatSynWhere
makeClassyPlate [] ''UPatternTypeSignature
makeClassyPlate [] ''URole
makeClassyPlate [] ''UCmd
makeClassyPlate [ Right '_langExt ] ''ULanguageExtension
makeClassyPlate [] ''UMatchLhs

-- Literal
makeClassyPlate [ Right '_charLitValue, Right '_stringLitValue, Right '_intLitValue, Right '_fracLitValue, Right '_floatLitValue ] ''ULiteral
makeClassyPlate [ Right '_promotedIntValue, Right '_promotedStringValue ] ''UPromoted

-- Base
makeClassyPlate [] ''UOperator
makeClassyPlate [] ''UName
makeClassyPlate [] ''UQualifiedName
makeClassyPlate [ Right '_moduleNameString ] ''UModuleName
makeClassyPlate [ Right '_simpleNameStr ] ''UNamePart
makeClassyPlate [ Right '_stringNodeStr ] ''UStringNode
makeClassyPlate [] ''UDataOrNewtypeKeyword
makeClassyPlate [] ''UDoKind
makeClassyPlate [] ''TypeKeyword
makeClassyPlate [] ''UOverlapPragma
makeClassyPlate [] ''UCallConv
makeClassyPlate [] ''UArrowAppl
makeClassyPlate [] ''USafety
makeClassyPlate [] ''UConlikeAnnot
makeClassyPlate [] ''Assoc
makeClassyPlate [ Right '_precedenceValue ] ''Precedence
makeClassyPlate [ Right '_lineNumber ] ''LineNumber
makeClassyPlate [] ''UPhaseControl
makeClassyPlate [ Right '_phaseNum ] ''PhaseNumber
makeClassyPlate [] ''PhaseInvert