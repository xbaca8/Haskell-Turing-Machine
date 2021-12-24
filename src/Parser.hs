{-|
Module       : TM Parser
Description  : A parser for TM .txt descriptions
-}

module Parser (module Parser, parseFile) where

import TM_AST
import ParserCombinators


turingMachine :: Parser TuringMachine
turingMachine = some element <+-> whitespace

-- | A alphabet symbol is either 0, 1, or _.
alphabet :: Parser Alphabet
alphabet =     (sym '0'                           >>: Zero)
           <|> (sym '1'                           >>: One)
           <|> (sym '_'                           >>: Blank)
           <???> "<alphabet> expected"

move :: Parser Move
move =         (sym 'L'                           >>: Lt)
           <|> (sym 'R'                           >>: Rt)
           <|> (sym 'S'                           >>: Stay)
           <???> "<move> expected"

state :: Parser State
state =        (text "State" <-+> number           >>=: \n -> State n)
           <|> (text "Halt"                        >>: Halt)

actionZero :: Parser Action
actionZero =      (text "read" <-+> sym '0')
             <-+> (text "write:" <-+> alphabet) 
              <+> (text "move:" <-+> move)
              <+> (text "goto:" <-+> state)        >>=: \((w0, m0), s0)
                                                    -> Action Zero w0 m0 s0

actionOne :: Parser Action
actionOne =      (text "read" <-+> sym '1')
             <-+> (text "write:" <-+> alphabet) 
              <+> (text "move:" <-+> move)
              <+> (text "goto:" <-+> state)        >>=: \((w1, m1), s1)
                                                    -> Action One w1 m1 s1
                                                
actionBlank :: Parser Action
actionBlank =      (text "read" <-+> sym '_')
             <-+> (text "write:" <-+> alphabet) 
              <+> (text "move:" <-+> move)
              <+> (text "goto:" <-+> state)        >>=: \((wb, mb), sb)
                                                    -> Action Blank wb mb sb

element :: Parser Element
element = (text "state:" <-+> number 
            <+> actionZero
            <+> actionOne
            <+> actionBlank                       >>=: \(((n, a0), a1), ab) 
                                                    -> StateBlock n a0 a1 ab )
    <|> (text "tapeInit: " <-+> many alphabet     >>=: \tapelist
                                                    -> TapeBlock tapelist)
