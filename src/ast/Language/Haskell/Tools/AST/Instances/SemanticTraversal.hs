-- | Generating instances for traversing the semantic information of the Haskell Representation
{-# LANGUAGE TemplateHaskell
           #-}
module Language.Haskell.Tools.AST.Instances.SemanticTraversal where

import Language.Haskell.Tools.AST.TH.SemanticTraversal
import Control.Applicative

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


-- Modules
deriveSemanticTraversal ''UModule
deriveSemanticTraversal ''UModuleHead
deriveSemanticTraversal ''UExportSpecs
deriveSemanticTraversal ''UExportSpec
deriveSemanticTraversal ''UIESpec
deriveSemanticTraversal ''USubSpec
deriveSemanticTraversal ''UModulePragma
deriveSemanticTraversal ''UFilePragma
deriveSemanticTraversal ''UImportDecl
deriveSemanticTraversal ''UImportSpec
deriveSemanticTraversal ''UImportQualified
deriveSemanticTraversal ''UImportSource
deriveSemanticTraversal ''UImportSafe
deriveSemanticTraversal ''UTypeNamespace
deriveSemanticTraversal ''UImportRenaming

-- Declarations
deriveSemanticTraversal ''UDecl
deriveSemanticTraversal ''UClassBody
deriveSemanticTraversal ''UClassElement
deriveSemanticTraversal ''UDeclHead
deriveSemanticTraversal ''UInstBody
deriveSemanticTraversal ''UInstBodyDecl
deriveSemanticTraversal ''UGadtConDecl
deriveSemanticTraversal ''UGadtConType
deriveSemanticTraversal ''UFieldWildcard
deriveSemanticTraversal ''UFunDeps
deriveSemanticTraversal ''UFunDep
deriveSemanticTraversal ''UConDecl
deriveSemanticTraversal ''UFieldDecl
deriveSemanticTraversal ''UDeriving
deriveSemanticTraversal ''UInstanceRule
deriveSemanticTraversal ''UInstanceHead
deriveSemanticTraversal ''UTypeEqn
deriveSemanticTraversal ''UKindConstraint
deriveSemanticTraversal ''UTyVar
deriveSemanticTraversal ''UType
deriveSemanticTraversal ''UKind
deriveSemanticTraversal ''UContext
deriveSemanticTraversal ''UAssertion
deriveSemanticTraversal ''UExpr
deriveSemanticTraversal ''UCompStmt
deriveSemanticTraversal ''UValueBind
deriveSemanticTraversal ''UPattern
deriveSemanticTraversal ''UPatternField
deriveSemanticTraversal ''USplice
deriveSemanticTraversal ''QQString
deriveSemanticTraversal ''UMatch
deriveSemanticTraversal ''URhs
deriveSemanticTraversal ''UGuardedRhs
deriveSemanticTraversal ''UFieldUpdate
deriveSemanticTraversal ''UBracket
deriveSemanticTraversal ''UTopLevelPragma
deriveSemanticTraversal ''URule
deriveSemanticTraversal ''UAnnotationSubject
deriveSemanticTraversal ''UMinimalFormula
deriveSemanticTraversal ''UExprPragma
deriveSemanticTraversal ''USourceRange
deriveSemanticTraversal ''Number
deriveSemanticTraversal ''UQuasiQuote
deriveSemanticTraversal ''URhsGuard
deriveSemanticTraversal ''ULocalBind
deriveSemanticTraversal ''ULocalBinds
deriveSemanticTraversal ''UFixitySignature
deriveSemanticTraversal ''UTypeSignature
deriveSemanticTraversal ''UListCompBody
deriveSemanticTraversal ''UTupSecElem
deriveSemanticTraversal ''UTypeFamily
deriveSemanticTraversal ''UTypeFamilySpec
deriveSemanticTraversal ''UInjectivityAnn
deriveSemanticTraversal ''UPatternSynonym
deriveSemanticTraversal ''UPatSynRhs
deriveSemanticTraversal ''UPatSynLhs
deriveSemanticTraversal ''UPatSynWhere
deriveSemanticTraversal ''UPatternTypeSignature
deriveSemanticTraversal ''URole
deriveSemanticTraversal ''UCmd
deriveSemanticTraversal ''ULanguageExtension
deriveSemanticTraversal ''UMatchLhs
deriveSemanticTraversal ''UStmt'
deriveSemanticTraversal ''UAlt'
deriveSemanticTraversal ''UCaseRhs'
deriveSemanticTraversal ''UGuardedCaseRhs'

-- ULiteral
deriveSemanticTraversal ''ULiteral
deriveSemanticTraversal ''UPromoted

-- Base
deriveSemanticTraversal ''UOperator
deriveSemanticTraversal ''UName
deriveSemanticTraversal ''UQualifiedName
deriveSemanticTraversal ''UModuleName
deriveSemanticTraversal ''UNamePart
deriveSemanticTraversal ''UStringNode
deriveSemanticTraversal ''UDataOrNewtypeKeyword
deriveSemanticTraversal ''UDoKind
deriveSemanticTraversal ''TypeKeyword
deriveSemanticTraversal ''UOverlapPragma
deriveSemanticTraversal ''UCallConv
deriveSemanticTraversal ''UArrowAppl
deriveSemanticTraversal ''USafety
deriveSemanticTraversal ''UConlikeAnnot
deriveSemanticTraversal ''Assoc
deriveSemanticTraversal ''Precedence
deriveSemanticTraversal ''LineNumber
deriveSemanticTraversal ''UPhaseControl
deriveSemanticTraversal ''PhaseNumber
deriveSemanticTraversal ''PhaseInvert
