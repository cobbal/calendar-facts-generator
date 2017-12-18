module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Control.Monad.Eff.JQuery as JQ
import DOM (DOM)

import Data.Char.Unicode (isDigit, isLower, isUpper)
import Data.List (List(..), (:))
import Data.List.NonEmpty (toList)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Function (on)
import Data.Tuple (Tuple(..), fst, snd)

import Data.List as L
import Data.Array as A
import Data.String as S

updatePage :: forall e. Eff (dom :: DOM, random :: RANDOM | e) Unit
updatePage = do
  Tuple pf sf <- createExtension
  JQ.setText pf =<< JQ.select "#prefix"
  JQ.setText sf =<< JQ.select "#suffix"

main :: forall e. Eff (dom :: DOM, random :: RANDOM | e) Unit
main = JQ.ready $ do
  updatePage
  JQ.on "click" (\_ _ -> updatePage) =<< JQ.select "#genButton"

type String' = List Char

extStrs :: Array String
extStrs = [ "OverlappingInstances"
          , "UndecidableInstances"
          , "IncoherentInstances"
          , "DoRec"
          , "RecursiveDo"
          , "ParallelListComp"
          , "MultiParamTypeClasses"
          , "MonomorphismRestriction"
          , "FunctionalDependencies"
          , "Rank2Types"
          , "RankNTypes"
          , "PolymorphicComponents"
          , "ExistentialQuantification"
          , "ScopedTypeVariables"
          , "PatternSignatures"
          , "ImplicitParams"
          , "FlexibleContexts"
          , "FlexibleInstances"
          , "EmptyDataDecls"
          , "CPP"
          , "KindSignatures"
          , "BangPatterns"
          , "TypeSynonymInstances"
          , "TemplateHaskell"
          , "ForeignFunctionInterface"
          , "Arrows"
          , "Generics"
          , "ImplicitPrelude"
          , "NamedFieldPuns"
          , "PatternGuards"
          , "GeneralizedNewtypeDeriving"
          , "ExtensibleRecords"
          , "RestrictedTypeSynonyms"
          , "HereDocuments"
          , "MagicHash"
          , "TypeFamilies"
          , "StandaloneDeriving"
          , "UnicodeSyntax"
          , "UnliftedFFITypes"
          , "InterruptibleFFI"
          , "CApiFFI"
          , "LiberalTypeSynonyms"
          , "TypeOperators"
          , "RecordWildCards"
          , "RecordPuns"
          , "DisambiguateRecordFields"
          , "TraditionalRecordSyntax"
          , "OverloadedStrings"
          , "GADTs"
          , "GADTSyntax"
          , "MonoPatBinds"
          , "RelaxedPolyRec"
          , "ExtendedDefaultRules"
          , "UnboxedTuples"
          , "DeriveDataTypeable"
          , "DeriveGeneric"
          , "DefaultSignatures"
          , "InstanceSigs"
          , "ConstrainedClassMethods"
          , "PackageImports"
          , "ImpredicativeTypes"
          , "NewQualifiedOperators"
          , "PostfixOperators"
          , "QuasiQuotes"
          , "TransformListComp"
          , "MonadComprehensions"
          , "ViewPatterns"
          , "XmlSyntax"
          , "RegularPatterns"
          , "TupleSections"
          , "GHCForeignImportPrim"
          , "NPlusKPatterns"
          , "DoAndIfThenElse"
          , "MultiWayIf"
          , "LambdaCase"
          , "RebindableSyntax"
          , "ExplicitForAll"
          , "DatatypeContexts"
          , "MonoLocalBinds"
          , "DeriveFunctor"
          , "DeriveTraversable"
          , "DeriveFoldable"
          , "NondecreasingIndentation"
          , "SafeImports"
          , "Safe"
          , "Trustworthy"
          , "Unsafe"
          , "ConstraintKinds"
          , "PolyKinds"
          , "DataKinds"
          , "ParallelLists"
          , "RoleAnnotations"
          , "OverloadedLists"
          , "EmptyCase"
          , "AutoDeriveTypeable"
          , "NegativeLiterals"
          , "BinaryLiterals"
          , "NumDecimals"
          , "NullaryTypeClasses"
          , "ExplicitNamespaces"
          , "AllowAmbiguousTypes"
          , "JavaScriptFFI"
          , "PatternSynonyms"
          , "PartialTypeSignatures"
          , "NamedWildCards"
          , "DeriveAnyClass"
          , "DeriveLift"
          , "StaticPointers"
          , "StrictData"
          , "Strict"
          , "ApplicativeDo"
          , "DuplicateRecordFields"
          , "TypeApplications"
          , "TypeInType"
          , "UndecidableSuperClasses"
          , "MonadFailDesugaring"
          , "TemplateHaskellQuotes"
          , "OverloadedLabels"]

exts :: List String'
exts = A.toUnfoldable $ stringToList <$> extStrs

-- camel-uncasing
-- many thanks to https://github.com/fatih/camelcase/blob/master/camelcase.go
classifyChar :: Char -> Int
classifyChar c
  | isUpper c = 0
  | isLower c = 1
  | isDigit c = 2
  | otherwise = 3

init' :: forall a. List a -> List a
init' l = fromMaybe Nil (L.init l)

tail' :: forall a. List a -> List a
tail' l = fromMaybe Nil (L.tail l)

listToString :: String' -> String
listToString l = S.fromCharArray (A.fromFoldable l)

stringToList :: String -> String'
stringToList s = A.toUnfoldable (S.toCharArray s)

chopCamel :: String' -> List String'
chopCamel x = fixup (toList <$> L.groupBy ((==) `on` classifyChar) x)
  where
    fixup :: List (String') -> List (String')
    fixup Nil = Nil
    fixup (Nil : s) = fixup s
    fixup (s0@(c0 : _) : s1@(c1 : _) : ss)
      | isUpper c0 && isLower c1 =
          fixup (init' s0 : (fromMaybe '?' (L.last s0) : s1) : ss)
    fixup (s0 : ss) = s0 : fixup ss

-- back to extensions

riggedChopCamel :: String' -> List String'
riggedChopCamel s
  | s == stringToList "GADTs" = s : Nil -- GAD Ts follow no one's rules!
  | otherwise = chopCamel s

splits :: forall a. List a -> List (Tuple (List a) (List a))
splits Nil = Tuple Nil Nil : Nil
splits (x : xs) = Tuple Nil (x : xs) : do
  Tuple p s <- splits xs
  pure (Tuple (x : p) s)

nonTrivialSplits :: forall a. List a -> List (Tuple (List a) (List a))
nonTrivialSplits (x : Nil) = splits (x : Nil)
nonTrivialSplits l = init' (tail' (splits l))

randomElem :: forall e a. List a -> Eff (random :: RANDOM | e) (Maybe a)
randomElem l = do
  let a = A.fromFoldable l
  if A.length a > 0
    then A.index a <$> randomInt 0 (A.length a - 1)
    else pure Nothing

extFixes :: Tuple (List (List String')) (List (List String'))
extFixes = L.unzip $ L.concat $ nonTrivialSplits <$> (riggedChopCamel <$> exts)

extPrefixes :: List String
extPrefixes = L.delete "" $ listToString <$> (L.nub $ L.concat <$> fst extFixes)

extSuffixes :: List String
extSuffixes = L.delete "" $ listToString <$> (L.nub $ L.concat <$> snd extFixes)

magic :: forall e. Eff (random :: RANDOM | e) (Maybe (Tuple String String))
magic = do
  pf <- randomElem extPrefixes
  sf <- randomElem extSuffixes
  pure (Tuple <$> pf <*> sf)

untilM :: forall m a b. Monad m => (a -> Maybe b) -> m a -> m b
untilM p m = do
  x <- (p <$> m)
  case x of
    Just r -> pure r
    Nothing -> untilM p m

approveExtension :: Tuple String String -> Maybe (Tuple String String)
approveExtension s = if L.elem (fst s <> snd s) extStrs then Nothing else Just s

createExtension :: forall e. Eff (random :: RANDOM | e) (Tuple String String)
createExtension = untilM ((=<<) approveExtension) magic

-- main :: IO ()
-- main = putStrLn =<< createExtension
