{-# LANGUAGE LambdaCase #-}
module Sequent_conclusion where
import Control.Monad
import Data.List
import System.Process -- add "process" package in dependencies

{-
  This program works on Ubuntu 18.04, GraphViz 2.40.1
-}

data Sequent = Turnstile [Formula] [Formula]
  deriving (Eq)

instance Show Sequent where
  show (Turnstile xls xrs) = show xls ++ "⊢" ++ show xrs

data Formula = Var String
             | Not Formula
             | And Formula Formula
             | Or Formula Formula
             | Impl Formula Formula
  deriving (Eq)

instance Show Formula where
  show (Var name) = name
  show (Not formula) = "(¬" ++ show formula ++ ")"
  show (And formulal formular) = "(" ++ show formulal ++ "∧" ++ show formular ++ ")"
  show (Or formulal formular) = "(" ++ show formulal ++ "∨" ++ show formular ++ ")"
  show (Impl formulal formular) = "(" ++ show formulal ++ "→" ++ show formular ++ ")"

{-
  Checks if all formulas in a Sequent are variables.
  If no, it starts the eval.
  If yes, it records this Sequent in the log and ends.
-}
checker :: (Sequent, String) -> [(Sequent, String)]
checker (s@(Turnstile xls xrs), str) | not (all isVar xrs) = evalRight (nubInSequence s, str)
                                     | not (all isVar xls) = evalLeft (nubInSequence s, str)
                                     | otherwise = [(nubInSequence s, "\"" ++ show (nubInSequence s) ++ "\"" ++ str)]
  where
    isVar :: Formula -> Bool
    isVar = \case
      Var str -> True
      _ -> False


--Rules of introduction to the succedent.
evalRight :: (Sequent, String) -> [(Sequent, String)]
evalRight = \case
  (s@(Turnstile xls (xr:xrs)), str) -> case xr of
    Var name -> checker (Turnstile xls (xrs ++ [xr]), str)
    Not formula -> checker (Turnstile (formula:xls) xrs, " -> " ++ "\"" ++show s ++ "\"" ++ str)
    And formulal formular -> checker (Turnstile xls (formulal:xrs), " -> " ++ "\"" ++show s ++ "\"" ++ str) ++ checker (Turnstile xls (formular:xrs), " -> " ++ "\"" ++show s ++ "\"" ++ str)
    Or formulal formular -> checker (Turnstile xls (formulal:(formular:xrs)), " -> " ++ "\"" ++show s ++ "\"" ++ str)
    Impl formulal formular -> checker (Turnstile (formulal:xls) (formular:xrs), " -> " ++ "\"" ++show s ++ "\"" ++ str)

--Rules of introduction to the antecedent
evalLeft :: (Sequent, String) -> [(Sequent, String)]
evalLeft = \case
  (s@(Turnstile (xl:xls) xrs), str) -> case xl of
    Var name -> checker (Turnstile (xls ++ [xl]) xrs, str)
    Not formula -> checker (Turnstile xls (formula:xrs), " -> " ++ "\"" ++show s ++ "\"" ++ str)
    And formulal formular -> checker (Turnstile (formulal:(formular:xls)) xrs, " -> " ++ "\"" ++show s ++ "\"" ++ str)
    Or formulal formular -> checker (Turnstile (formulal:xls) xrs, " -> " ++ "\"" ++show s ++ "\"" ++ str) ++ checker (Turnstile (formular:xls) xrs, " -> " ++ "\"" ++show s ++ "\"" ++ str)
    Impl formulal formular-> checker (Turnstile (formular:xls) xrs, " -> " ++ "\"" ++show s ++ "\"" ++ str) ++ checker (Turnstile xls (formulal:xrs), " -> " ++ "\"" ++show s ++ "\"" ++ str)

--Removes duplicate formulas separately in the antecedent and succedent sequent
--Asymptotics O(n^2) - It can be improved, but it'll do.
nubInSequence :: Sequent -> Sequent
nubInSequence (Turnstile xls xrs) = Turnstile (nub xls) (nub xrs)

{-
  Checks every leaf of the initial sequent conclusion tree for validity.
  Returns either the ((not valid message, log - branch to not valid leaf),all logs - branches) or the (valid message, all logs - branches)
-}
checkValidList :: [(Sequent, String)] -> Either ((String, String), [String]) (String, [String])
checkValidList xs | all checkValid xs = Right ("This sequent is valid!", map snd xs)
                  | otherwise = Left (("The sequent is not valid! \\n Counterexample - " ++ cntrEx (fst notValidSequence), snd notValidSequence), map snd xs)
    where
      notValidSequence :: (Sequent, String)
      notValidSequence = head (dropWhile checkValid xs)

--Checks a sequent for validity
checkValid :: (Sequent, String) -> Bool
checkValid (Turnstile xls xrs, str) | hasAny xls xrs = True
                                    | otherwise = False

--Returns true if the given list contains any of the elements in the search list.
--List of elements to look for -> List to search -> Result
hasAny :: Eq a => [a] -> [a] -> Bool
hasAny [] _ = False
hasAny _ [] = False
hasAny search (x:xs) = if x `elem` search then True else hasAny search xs

--Build a string with a counterexample
cntrEx :: Sequent -> String
cntrEx (Turnstile xls xrs) = "True: " ++ concatMap show xls ++ "; False: " ++ concatMap show xrs

{-
  Launch function.
  According to the log of calculations, a file is constructed for GraphViz Ubuntu.
  With a system call, we create a pdf with an answer and a conclusion tree.
  If the Sequent is valid, then a message about this and a graph are displayed.
  If the Sequent is not valid, then a message about this, a counterexample and a graph, is displayed.
  Bold arrows indicate a branch to leaf with a counterexample.
-}
main :: Sequent -> IO()
main s = case checkValidList (checker (s,"")) of
               Right (str, logs) -> do
                                    writeFile "result.dot" validStr
                                    system "dot -Tpdf result.dot -o result.pdf"
                                    putStrLn "See result in result.pdf"
                where
                  validStr :: String
                  validStr = "strict digraph G {\n\tnode[shape=\"rectangle\", style=rounded]\n\tlabel = \"" ++ str ++ "\"\n\tlabelloc = top" ++ unlinetab logs ++ "\n }"

                  unlinetab :: [String] -> String
                  unlinetab [] = []
                  unlinetab (x:xs) = "\n\t" ++ x ++ unlinetab xs
               Left ((str, cntr), logs) -> do
                                           writeFile "result.dot" notValidStr
                                           system "dot -Tpdf result.dot -o result.pdf"
                                           putStrLn "See result in result.pdf"
                where
                  notValidStr :: String
                  notValidStr = "strict digraph G {\n\tnode[shape=\"rectangle\", style=rounded]\n\t label = \"" ++ str ++ "\"\n\t labelloc = top" ++ unlinetab logs cntr ++ "\n }"

                  unlinetab :: [String] -> String -> String
                  unlinetab [] str = []
                  unlinetab (x:xs) str | x == str = "\n\t" ++ x ++ " [penwidth = 5]" ++ unlinetab xs str
                                       | otherwise = "\n\t" ++ x ++ unlinetab xs str

--Tests for the introduction of rules succedent
test1 = Turnstile [] [Var "a"]
test2 = Turnstile [] [Not (Var "a")]
test3 = Turnstile [] [And (Var "a") (Var "b")]
test4 = Turnstile [] [Or (Var "a") (Var "b")]
test5 = Turnstile [] [Impl (Var "a") (Var "b")]

--Tests for the introduction of rules antecedent
test6 = Turnstile [Var "a"] []
test7 = Turnstile [Not (Var "a")] []
test8 = Turnstile [And (Var "a") (Var "b")] []
test9 = Turnstile [Or (Var "a") (Var "b")] []
test10 = Turnstile [Impl (Var "a") (Var "b")] []

--Difficult test
test11 = Turnstile [] [Or (Not (And (Or (Not (Var "q")) (Var "p")) (Or (Not (Var "q")) (Var "q")))) (Not (Or (And (Var "p") (Not (Var "q"))) (And (Var "q") (Not (Var "p")))))]
