{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Control.Monad.Eff.JQuery as JQ
import DOM (DOM)

import Data.Traversable (traverse)
import Data.Maybe (Maybe(..), fromMaybe)

import Data.Array as A
import Data.String as S

updatePage :: forall e. Eff (dom :: DOM, random :: RANDOM | e) Unit
updatePage = do
  usePathSampler <- JQ.hasClass "path" =<< JQ.select "#sampler > :selected"
  phrase <- if usePathSampler then randomPhrasePathy else randomPhraseBranchy
  JQ.setText phrase =<< JQ.select "#fact"

main :: forall e. Eff (dom :: DOM, random :: RANDOM | e) Unit
main = JQ.ready $ do
  updatePage
  JQ.on "click" (\_ _ -> updatePage) =<< JQ.select "#genButton"

data Phrase = Str String
            | Seq (Array Phrase)
            | Choice (Array Phrase)

instance showPhrase :: Show Phrase where
  show p@(Str s) = show (phraseWeight p) <> ": " <> show s
  show p@(Seq l) = show (phraseWeight p) <> ": " <> "Seq [" <> S.joinWith "\n, " (show <$> l) <> "\n]"
  show p@(Choice l) = show (phraseWeight p) <> ": " <> "Choice [" <> S.joinWith "\n, " (show <$> l) <> "\n]"

root :: Phrase
root = Seq [ Str "Did you know that"
           , Choice [ Seq [ Str "the"
                          , Choice [ Str "Fall", Str "Spring" ]
                          , Str "Equinox"
                          ]
                    , Seq [ Str "the"
                          , Choice [ Str "Winter", Str "Summer" ]
                          , Choice [ Str "Solstice", Str "Olympics" ]
                          ]
                    , Seq [ Str "the"
                          , Choice [ Str "Earliest", Str "Latest" ]
                          , Choice [ Str "Sunrise", Str "Sunset" ]
                          ]
                    , Seq [ Str "Daylight"
                          , Choice [ Str "Saving", Str "Savings" ]
                          , Str "Time"
                          ]
                    , Seq [ Str "Leap"
                          , Choice [ Str "Day", Str "Year" ]
                          ]
                    , Str "Easter"
                    , Seq [ Str "the"
                          , Choice [Str "Harvest", Str "Super", Str "Blood"]
                          , Str "Moon"
                          ]
                    , Str "Toyota Truck Month"
                    , Str "Shark Week"
                    ]
           , Choice [ Seq [ Str "happens"
                          , Choice [ Str "earlier"
                                   , Str "later"
                                   , Str "at the wrong time"
                                   ]
                          , Str "every year"
                          ]
                    , Seq [ Str "drifts out of sync with the"
                          , Choice [ Str "Sun"
                                   , Str "Moon"
                                   , Str "Zodiac"
                                   , Seq [ Choice [ Str "Gregorian"
                                                  , Str "Mayan"
                                                  , Str "Lunar"
                                                  , Str "iPhone"
                                                  ]
                                         , Str "Calendar"
                                         ]
                                   , Str "atomic clock in Colorado"
                                   ]
                          ]
                    , Seq [ Str "might"
                          , Choice [ Str "not happen"
                                   , Str "happen twice"
                                   ]
                          , Str "this year"
                          ]
                    ]
           , Str "because of"
           , Choice [ Seq [ Str "time zone legislation in"
                          , Choice [ Str "Indiana"
                                   , Str "Arizona"
                                   , Str "Russia"
                                   ]
                          ]
                    , Str "a decree by the pope in the 1500s"
                    , Seq [ Choice [ Str "precession"
                                   , Str "libration"
                                   , Str "nutation"
                                   , Str "libation"
                                   , Str "eccentricity"
                                   , Str "obliquity"
                                   ]
                          , Str "of the"
                          , Choice [ Str "Moon"
                                   , Str "Sun"
                                   , Str "Earth's axis"
                                   , Str "equator"
                                   , Str "prime meridian"
                                   , Seq [Choice [ Str "international date"
                                                 , Str "mason-dixon"
                                                 ]
                                         , Str "line"]
                                   ]
                          ]
                    , Str "magnetic field reversal"
                    , Seq [ Str "an arbitrary decision by"
                          , Choice [ Str "Benjamin Franklin"
                                   ,Str "Isaac Newton"
                                   ,Str "FDR"
                                   ]
                          ]
                    ]
           , Str "?"
           , Str "Apparently"
           , Choice [ Str "it causes a predictable increase in car accidents."
                    , Str "that's why we have leap seconds."
                    , Str "scientists are really worried."
                    , Seq [ Str "it was even more extreme during the"
                          , Choice [ Str "Bronze Age."
                                   , Str "Ice Age."
                                   , Str "Cretaceous."
                                   , Str "1990s."
                                   ]
                          ]
                    , Seq [ Str "there's a proposal to fix it, but it"
                          , Choice [ Str "will never happen."
                                   , Str "actually makes things worse."
                                   , Str "is stalled in congress."
                                   , Str "might be unconstitutional."
                                   ]
                          ]
                    , Str "it's getting worse and no one knows why."
                    ]
           ]

randomElem :: forall e a. Array a -> Eff (random :: RANDOM | e) (Maybe a)
randomElem a = A.index a <$> randomInt 0 (A.length a - 1)

randomWeightedElem :: forall e a. Array a -> Array Int -> Eff (random :: RANDOM | e) (Maybe a)
randomWeightedElem arr ws = lookupByWeight <$> randomInt 0 (totalWeight - 1)
  where
    runningWeights :: Array Int
    runningWeights = gen 0 0
      where
        gen :: Int -> Int -> Array Int
        gen w i =
          case A.index ws i of
            Nothing -> []
            Just w' -> A.cons (w + w') (gen (w + w') (i + 1))

    totalWeight :: Int
    totalWeight = fromMaybe 0 $ A.last runningWeights

    lookupByWeight :: Int -> Maybe a
    lookupByWeight r = A.index arr =<< A.findIndex (\w -> r < w) runningWeights

randomPhraseBranchy :: forall e. Eff (random :: RANDOM | e) String
randomPhraseBranchy = gen root
  where
    gen :: Phrase -> Eff (random :: RANDOM | e) String
    gen (Str s) = pure s
    gen (Seq l) = S.joinWith " " <$> traverse gen l
    gen (Choice l) = gen <<< fromMaybe (Str "") =<< randomElem l

phraseWeight :: Phrase -> Int
phraseWeight (Str _) = 1
phraseWeight (Seq l) = A.foldl (*) 1 (phraseWeight <$> l)
phraseWeight (Choice l) = A.foldl (+) 0 (phraseWeight <$> l)

randomPhrasePathy :: forall e. Eff (random :: RANDOM | e) String
randomPhrasePathy = gen root
  where
    gen :: Phrase -> Eff (random :: RANDOM | e) String
    gen (Str s) = pure s
    gen (Seq l) = S.joinWith " " <$> traverse gen l
    gen (Choice l) = gen <<< fromMaybe (Str "") =<< randomWeightedElem l (phraseWeight <$> l)
