module AST where

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

type Log = String
