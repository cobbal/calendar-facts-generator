{-# LANGUAGE Rank2Types #-}
import Language.Haskell.Extension
import Data.Char
import Data.List
import Data.Function
import System.Random (randomRIO)

exts :: [KnownExtension]
exts = filter (`notElem` blacklist) [minBound ..]
  where blacklist = []


-- camel-uncasing
-- many thanks to https://github.com/fatih/camelcase/blob/master/camelcase.go
data CharType = UpperLet | LowerLet | Digit | Other
              deriving (Eq)

classifyChar :: Char -> CharType
classifyChar c
  | isUpper c = UpperLet
  | isLower c = LowerLet
  | isDigit c = Digit
  | otherwise = Other

chopCamel :: String -> [String]
chopCamel = fixup . groupBy ((==) `on` classifyChar)
  where
    fixup :: [String] -> [String]
    fixup [] = []
    fixup ("" : s) = fixup s
    fixup (s0@(c0 : _) : s1@(c1 : _) : ss)
      | isUpper c0 && isLower c1 =
        fixup (init s0 : (last s0 : s1) : ss)
    fixup (s0 : ss) = s0 : fixup ss

camelTests :: [(String, [String])]
camelTests = [ ("" ,                     [])
             , ("lowercase" ,            ["lowercase"])
             , ("Class" ,                ["Class"])
             , ("MyClass" ,              ["My", "Class"])
             , ("MyC" ,                  ["My", "C"])
             , ("HTML" ,                 ["HTML"])
             , ("PDFLoader" ,            ["PDF", "Loader"])
             , ("AString" ,              ["A", "String"])
             , ("SimpleXMLParser" ,      ["Simple", "XML", "Parser"])
             , ("vimRPCPlugin" ,         ["vim", "RPC", "Plugin"])
             , ("GL11Version" ,          ["GL", "11", "Version"])
             , ("99Bottles" ,            ["99", "Bottles"])
             , ("May5" ,                 ["May", "5"])
             , ("BFG9000" ,              ["BFG", "9000"])
             , ("BöseÜberraschung" ,     ["Böse", "Überraschung"])
             , ("Two  spaces" ,          ["Two", "  ", "spaces"])]

runTests :: IO ()
runTests = flip mapM_ camelTests $ \(i, expected) -> do
  let actual = chopCamel i
  if actual == expected
    then pure ()
    else do
    putStrLn $ "test failure for (chopCaml " ++ show i ++ ")"
    putStrLn $ "expected: " ++ show expected
    putStrLn $ "  acutal: " ++ show actual

-- back to extensions

riggedChopCamel :: String -> [String]
riggedChopCamel "GADTs" = ["GADTs"] -- GAD Ts follow no one's rules!
riggedChopCamel s = chopCamel s

splits :: [a] -> [([a], [a])]
splits [] = [([], [])]
splits (x : xs) = ([], x : xs) : do
  (p, s) <- splits xs
  [(x : p, s)]

nonTrivialSplits :: [a] -> [([a], [a])]
nonTrivialSplits [x] = splits [x]
nonTrivialSplits l = init (tail (splits l))

randomElem :: [a] -> IO a
randomElem l = (l !!) <$> randomRIO (0, length l - 1)

extStrs :: [String]
extStrs = show <$> exts

extFixes :: ([[String]], [[String]])
extFixes = unzip $ concat $ nonTrivialSplits . riggedChopCamel <$> extStrs

extPrefixes :: [String]
extPrefixes = nub $ concat <$> fst extFixes

extSuffixes :: [String]
extSuffixes = nub $ concat <$> snd extFixes

magic :: [[a]] -> [[a]] -> IO [a]
magic l0 l1 = (++) <$> randomElem l0 <*> randomElem l1

untilM :: Monad m => (a -> Bool) -> m a -> m a
untilM p m = do
  x <- m
  if p x then pure x else untilM p m

createExtension :: IO String
createExtension = untilM (`notElem` extStrs) (magic extPrefixes extSuffixes)

main :: IO ()
main = putStrLn =<< createExtension
