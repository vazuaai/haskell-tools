{-# LANGUAGE FlexibleContexts, TemplateHaskell #-}
-- Generated references for handling the custom AST
module Language.Haskell.Tools.AST.References where

import Control.Reference

import Language.Haskell.Tools.AST.Modules
import Language.Haskell.Tools.AST.TH
import Language.Haskell.Tools.AST.Decls
import Language.Haskell.Tools.AST.Binds
import Language.Haskell.Tools.AST.Exprs
import Language.Haskell.Tools.AST.Stmts
import Language.Haskell.Tools.AST.Patterns
import Language.Haskell.Tools.AST.Types
import Language.Haskell.Tools.AST.Kinds
import Language.Haskell.Tools.AST.Literals
import Language.Haskell.Tools.AST.Base
import Language.Haskell.Tools.AST.Ann

-- Modules
makeReferences ''Module
makeReferences ''ModuleHead
makeReferences ''ExportSpecList
makeReferences ''ExportSpec
makeReferences ''IESpec
makeReferences ''SubSpec
makeReferences ''ModulePragma
makeReferences ''ImportDecl
makeReferences ''ImportSpec
makeReferences ''ImportQualified
makeReferences ''ImportSource
makeReferences ''ImportSafe
makeReferences ''TypeNamespace
makeReferences ''ImportRenaming

-- Declarations
makeReferences ''Decl
makeReferences ''ClassBody
makeReferences ''ClassElement
makeReferences ''DeclHead
makeReferences ''InstBody
makeReferences ''InstBodyDecl
makeReferences ''GadtConDecl
makeReferences ''GadtConType
makeReferences ''GadtField
makeReferences ''FunDeps
makeReferences ''FunDep
makeReferences ''ConDecl
makeReferences ''FieldDecl
makeReferences ''Deriving
makeReferences ''InstanceRule
makeReferences ''InstanceHead
makeReferences ''TypeEqn
makeReferences ''KindConstraint
makeReferences ''TyVar
makeReferences ''Type
makeReferences ''Kind
makeReferences ''Context
makeReferences ''Assertion
makeReferences ''Expr
makeReferences ''Stmt'
makeReferences ''CompStmt
makeReferences ''ValueBind
makeReferences ''Pattern
makeReferences ''PatternField
makeReferences ''Splice
makeReferences ''QQString
makeReferences ''Match
makeReferences ''Alt'
makeReferences ''Rhs
makeReferences ''GuardedRhs
makeReferences ''FieldUpdate
makeReferences ''Bracket
makeReferences ''TopLevelPragma
makeReferences ''Rule
makeReferences ''AnnotationSubject
makeReferences ''MinimalFormula
makeReferences ''ExprPragma
makeReferences ''SourceRange
makeReferences ''Number
makeReferences ''QuasiQuote
makeReferences ''RhsGuard
makeReferences ''LocalBind
makeReferences ''LocalBinds
makeReferences ''FixitySignature
makeReferences ''TypeSignature
makeReferences ''ListCompBody
makeReferences ''TupSecElem
makeReferences ''TypeFamily
makeReferences ''TypeFamilySpec
makeReferences ''InjectivityAnn
makeReferences ''CaseRhs'
makeReferences ''GuardedCaseRhs'
makeReferences ''PatternSynonym
makeReferences ''PatSynRhs
makeReferences ''PatSynLhs
makeReferences ''PatSynWhere
makeReferences ''PatternTypeSignature
makeReferences ''Role
makeReferences ''LanguageExtension
makeReferences ''MatchLhs

-- Literal
makeReferences ''Literal
makeReferences ''Promoted

-- Base
makeReferences ''Operator
makeReferences ''Name
makeReferences ''SimpleName
makeReferences ''ModuleName
makeReferences ''UnqualName
makeReferences ''StringNode
makeReferences ''DataOrNewtypeKeyword
makeReferences ''DoKind
makeReferences ''TypeKeyword
makeReferences ''OverlapPragma
makeReferences ''CallConv
makeReferences ''ArrowAppl
makeReferences ''Safety
makeReferences ''Assoc
makeReferences ''Precedence
makeReferences ''PhaseControl
makeReferences ''PhaseNumber
makeReferences ''PhaseInvert