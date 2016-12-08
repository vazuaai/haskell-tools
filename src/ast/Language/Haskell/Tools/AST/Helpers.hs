{-# LANGUAGE FlexibleContexts 
           , LambdaCase 
           , RankNTypes 
           , ScopedTypeVariables
           , TypeFamilies
           , FlexibleInstances
           , UndecidableInstances
           , PatternSynonyms
           , TypeApplications
           , TupleSections
           , DataKinds
           , MultiParamTypeClasses
           , AllowAmbiguousTypes
           #-}

-- | Helper functions for using the AST.
module Language.Haskell.Tools.AST.Helpers where

import SrcLoc
import qualified Name as GHC

import Control.Reference
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Writer
import Data.List
import Data.Maybe
import Data.Function hiding ((&))
import Data.Generics.Uniplate.Operations
import Data.Generics.ClassyPlate
import Control.Instances.Morph

import Language.Haskell.Tools.AST.Ann
import Language.Haskell.Tools.AST.Representation.Modules
import Language.Haskell.Tools.AST.Representation.Decls
import Language.Haskell.Tools.AST.Representation.Patterns
import Language.Haskell.Tools.AST.Representation.Exprs
import Language.Haskell.Tools.AST.Representation.Binds
import Language.Haskell.Tools.AST.Representation.Types
import Language.Haskell.Tools.AST.Representation.Names
import Language.Haskell.Tools.AST.References
import Language.Haskell.Tools.AST.SemaInfoTypes
import Language.Haskell.Tools.AST.SemaInfoClasses


import Debug.Trace
 
-- | Does the import declaration import only the explicitly listed elements?
importIsExact :: Ann UImportDecl dom stage -> Bool
importIsExact = isJust . (^? importSpec&annJust&importSpecList)  

-- | Accesses the name of a function or value binding
bindingName :: (SemanticInfo dom UQualifiedName ~ ni) => Simple Traversal (Ann UValueBind dom stage) (Ann UQualifiedName dom stage)
bindingName = (valBindPat&patternName&simpleName 
                        &+& funBindMatches&annList&matchLhs
                              &(matchLhsName&simpleName &+& matchLhsOperator&operatorName))

-- | Accesses that name of a declaration through the declaration head.
declHeadNames :: Simple Traversal (Ann UDeclHead dom stage) (Ann UQualifiedName dom stage)
declHeadNames = (dhName&simpleName &+& dhBody&declHeadNames &+& dhAppFun&declHeadNames &+& dhOperator&operatorName)

-- | A reference to access type arguments to a type constructor call that may be universally qualified
-- or parenthesized.
typeParams :: Simple Traversal (Ann UType dom stage) (Ann UType dom stage)
typeParams = fromTraversal typeParamsTrav
  where typeParamsTrav f (Ann a (UTyFun p r)) = Ann a <$> (UTyFun <$> f p <*> typeParamsTrav f r)
        typeParamsTrav f (Ann a (UTyForall vs t)) = Ann a <$> (UTyForall vs <$> typeParamsTrav f t)
        typeParamsTrav f (Ann a (UTyCtx ctx t)) = Ann a <$> (UTyCtx ctx <$> typeParamsTrav f t)
        typeParamsTrav f (Ann a (UTyParen t)) = Ann a <$> (UTyParen <$> typeParamsTrav f t)
        typeParamsTrav f t = f t

valBindPats :: Simple Traversal (Ann UValueBind dom stage) (Ann UPattern dom stage)
valBindPats = valBindPat &+& funBindMatches & annList & matchLhs & (matchLhsArgs & annList &+& matchLhsLhs &+& matchLhsRhs)

-- | Access the semantic information of an AST node.
semantics :: Simple Lens (Ann elem dom stage) (SemanticInfo dom elem)
semantics = annotation&semanticInfo

-- | Get all nodes that contain a given source range
nodesContaining :: (HasRange (inner dom stage), Biplate (node dom stage) (inner dom stage), SourceInfo stage) 
                => RealSrcSpan -> Simple Traversal (node dom stage) (inner dom stage)
nodesContaining rng = biplateRef & filtered (isInside rng) 

-- | Return true if the node contains a given range
isInside :: HasRange a => RealSrcSpan -> a -> Bool
isInside rng nd = case getRange nd of RealSrcSpan sp -> sp `containsSpan` rng
                                      _              -> False

-- | Get all nodes that are contained in a given source range
nodesContained :: (HasRange (inner dom stage), Biplate (node dom stage) (inner dom stage), SourceInfo stage) 
                    => RealSrcSpan -> Simple Traversal (node dom stage) (inner dom stage)
nodesContained rng = biplateRef & filtered (containing rng) 

-- | Return true if the range contains a node
containing :: HasRange a => RealSrcSpan -> a -> Bool
containing rng nd = case getRange nd of RealSrcSpan sp -> rng `containsSpan` sp
                                        _              -> False

-- | Get the nodes that have exactly the given range 
nodesWithRange :: (Biplate (Ann node dom stage) (Ann inner dom stage), SourceInfo stage) 
               => RealSrcSpan -> Simple Traversal (Ann node dom stage) (Ann inner dom stage)
nodesWithRange rng = biplateRef & filtered (hasRange rng) 
  where -- True, if the node has the given range                     
        hasRange :: SourceInfo stage => RealSrcSpan -> Ann inner dom stage -> Bool
        hasRange rng node = case getRange node of RealSrcSpan sp -> sp == rng
                                                  _              -> False

-- | Get the shortest source range that contains the given 
getNodeContaining :: (Biplate (Ann node dom stage) (Ann inner dom stage), SourceInfo stage, HasRange (Ann inner dom stage)) 
                  => RealSrcSpan -> Ann node dom stage -> Maybe (Ann inner dom stage)
getNodeContaining sp node = case node ^? nodesContaining sp of
  [] -> Nothing
  results -> Just $ minimumBy (compareRangeLength `on` getRange) results

-- | Compares two source spans based on their lengths. Can only used for NESTED spans.
compareRangeLength :: SrcSpan -> SrcSpan -> Ordering
compareRangeLength (RealSrcSpan sp1) (RealSrcSpan sp2)
  = (lineDiff sp1 `compare` lineDiff sp2) `mappend` (colDiff sp1 `compare` colDiff sp2)
  where lineDiff sp = srcLocLine (realSrcSpanStart sp) - srcLocLine (realSrcSpanEnd sp)
        colDiff sp = srcLocCol (realSrcSpanStart sp) - srcLocCol (realSrcSpanEnd sp)

-- * Classyplate-based element selection

class HasRange t => BinaryFind a t where
  onNodes :: (a -> a) -> t -> t
  onNodesM :: Monad m => (a -> m a) -> t -> m t

instance (HasRange a, ApplyIf (TEQ s a) s a) => BinaryFind s a where
  onNodes = applyIf @(TEQ s a)
  {-# INLINE onNodes #-}
  onNodesM = applyIfM @(TEQ s a)
  {-# INLINE onNodesM #-}

class ApplyIf (sel :: Bool) x y where
  applyIf :: (x -> x) -> y -> y
  applyIfM :: Monad m => (x -> m x) -> y -> m y

instance ApplyIf True x x where
  applyIf f x = f x
  {-# INLINE applyIf #-}
  applyIfM f x = f x
  {-# INLINE applyIfM #-}

instance ApplyIf False x y where
  applyIf _ y = y
  {-# INLINE applyIf #-}
  applyIfM _ y = return y
  {-# INLINE applyIfM #-}

type family TEQ a b :: Bool where
  TEQ a a = True
  TEQ a b = False

type instance AppSelector (BinaryFind a) t = BinaryFindSelector t

type family BinaryFindSelector t :: Bool where
  BinaryFindSelector (Ann elem dom stage) = True
  BinaryFindSelector (AnnListG elem dom stage) = True
  BinaryFindSelector (AnnMaybeG elem dom stage) = True
  BinaryFindSelector other = False

-- | Evaluates the given function on the elements of the expected type that are containing a given range
onContaining :: forall root elem dom stage s . (SourceInfo stage, ClassyPlate (BinaryFind (elem dom stage)) (root dom stage)) 
             => RealSrcSpan -> (elem dom stage -> elem dom stage) -> root dom stage -> root dom stage
onContaining sp f = selectiveTraverse @(BinaryFind (elem dom stage)) (\e -> (onNodes f e, sp `isInside` e))

onContainingM :: forall root elem dom stage s m . (Monad m, SourceInfo stage, ClassyPlate (BinaryFind (elem dom stage)) (root dom stage)) 
              => RealSrcSpan -> (elem dom stage -> m (elem dom stage)) -> root dom stage -> m (root dom stage)
onContainingM sp f = selectiveTraverseM @(BinaryFind (elem dom stage)) (\e -> (onNodesM f e) >>= return . (, sp `isInside` e))

-- | Evaluates the given function on the elmeents of the expected type that are inside a given range
onContained :: forall root elem dom stage s . (SourceInfo stage, ClassyPlate (BinaryFind (elem dom stage)) (root dom stage)) 
            => RealSrcSpan -> (elem dom stage -> elem dom stage) -> root dom stage -> root dom stage
onContained sp f = selectiveTraverse @(BinaryFind (elem dom stage)) (\e -> (if sp `containing` e then onNodes f e else e, sp `isInside` e || sp `containing` e))

onContainedM :: forall root elem dom stage s m . (Monad m, SourceInfo stage, ClassyPlate (BinaryFind (elem dom stage)) (root dom stage)) 
             => RealSrcSpan -> (elem dom stage -> m (elem dom stage)) -> root dom stage -> m (root dom stage)
onContainedM sp f = selectiveTraverseM @(BinaryFind (elem dom stage)) (\e -> (if sp `containing` e then onNodesM f e else return e) >>= return . (, sp `isInside` e || sp `containing` e))


containedNodes :: (SourceInfo stage, ClassyPlate (BinaryFind (elem dom stage)) (root dom stage))
               => RealSrcSpan -> Simple Traversal (root dom stage) (elem dom stage)
containedNodes sp = createReference (onContainedM sp)

containingNodes :: (SourceInfo stage, ClassyPlate (BinaryFind (elem dom stage)) (root dom stage))
               => RealSrcSpan -> Simple Traversal (root dom stage) (elem dom stage)
containingNodes sp = createReference (onContainingM sp)


createReference :: (SourceInfo stage, ClassyPlate (BinaryFind (elem dom stage)) (root dom stage))
                => (forall m . Monad m => (elem dom stage -> m (elem dom stage)) -> root dom stage -> m (root dom stage)) 
                     -> Simple Traversal (root dom stage) (elem dom stage)
createReference traverse 
  = reference (morph . execWriter . traverse (\a -> tell [a] >> return a))
              (\b -> return . (runIdentity . traverse (\_ -> Identity b)))
              traverse

getContaining :: forall root elem dom stage s . (SourceInfo stage, ClassyPlate (BinaryFind (Ann elem dom stage)) (Ann root dom stage)) 
              => RealSrcSpan -> Ann root dom stage -> [Ann elem dom stage]
getContaining sp root = execWriter (onContainingM sp (\e -> tell [e] >> return e) root)

getContained :: forall root elem dom stage s . (SourceInfo stage, ClassyPlate (BinaryFind (Ann elem dom stage)) (Ann root dom stage)) 
             => RealSrcSpan -> Ann root dom stage -> [Ann elem dom stage]
getContained sp root = execWriter (onContainedM sp (\e -> tell [e] >> return e) root)


-- | A class to access the names of named elements. Have to locate where does the AST element store its name.
-- The returned name will be the one that was marked isDefining.
class NamedElement elem where
  elementName :: Simple Traversal (Ann elem dom st) (Ann UQualifiedName dom st)

instance NamedElement UDecl where
  elementName = (declHead & declHeadNames) 
                  &+& (declTypeFamily & tfHead & declHeadNames)
                  &+& (declValBind & bindingName)
                  &+& (declName & simpleName)
                  &+& (declPatSyn & patLhs & (patName & simpleName &+& patSynOp & operatorName))

instance NamedElement ULocalBind where
  elementName = localVal&bindingName &+& localSig&tsName&annList&simpleName

inScope :: GHC.Name -> Scope -> Bool
inScope n sc = any (n `elem`) sc

-- * Pattern synonyms for annotated lists and maybes
                        
pattern AnnList :: [Ann elem dom stage] -> AnnListG elem dom stage
pattern AnnList elems <- AnnListG _ elems

pattern AnnNothing :: AnnMaybeG elem dom stage
pattern AnnNothing <- AnnMaybeG _ Nothing

pattern AnnJust :: Ann elem dom stage -> AnnMaybeG elem dom stage
pattern AnnJust elem <- AnnMaybeG _ (Just elem)
