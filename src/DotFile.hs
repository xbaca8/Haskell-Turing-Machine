module DotFile where

import TM_AST
------------------------------
-- Types we may need: dotfile line
------------------------------
type DotFileLine = String


------------------------------------
-- Unnecessary but maybe used helpers
-- For general text we will see repeatedly (arrow and label)
-- and for converting abstract syntax to dot file concrete syntax
-- with alphaToStr, moveToStr, stateToStr, transistionStr
------------------------------------
arrowStr :: DotFileLine
arrowStr = " -> "

labelStr :: DotFileLine
labelStr = " [label = "

alphaToStr :: Alphabet -> String
alphaToStr Zero = "0"
alphaToStr One = "1"
alphaToStr Blank = "_"

moveToStr :: Move -> String
moveToStr Lt = "L"
moveToStr Rt = "R"
moveToStr Stay = "S"

stateToStr :: State -> String
stateToStr (State n) = show n
stateToStr Halt = "halt"

transitionStr :: Alphabet -> Alphabet -> Move -> DotFileLine
transitionStr currSym newSym moveTo = alphaToStr currSym ++ ":(" ++ alphaToStr newSym
                                         ++ "," ++ moveToStr moveTo ++ ")"

------------------------------------
-- Code to make dot file lines
------------------------------------
-- Helper for the code we expect to see at the beginning of any dot file
headerCode :: [DotFileLine]
headerCode = ["digraph D {", "init -> 0;" ]

-- Helper for the final code we should see at the end of a dotfile
trailerCode :: [DotFileLine]
trailerCode = [stateToStr Halt ++ arrowStr ++ stateToStr Halt ++ ";", "}"]

-- Used as base case for when we don't see any more text
noCode :: [DotFileLine]
noCode = []

-- State Code function
-- Somehow, given a state, it should then map lookup each character (0, 1, _)
--    with that state's stateblock with actions, and add those each to a block of
--    dotfile lines
-- | Recall: State Environment mapping (currentState, symbol seen) -> (newSym, Move, NewState)
stateCode :: Element -> [DotFileLine]
stateCode (StateBlock s (Action zSeen zWrite zMove zState) 
        (Action oSeen oWrite oMove oState) (Action bSeen bWrite bMove bState)) = 
    [
    show s ++ arrowStr ++ stateToStr zState ++ labelStr ++ "\"" ++ " " ++ 
            transitionStr Zero zWrite zMove ++ "\"" ++ "];",     -- State s see 0 
    show s ++ arrowStr ++ stateToStr oState ++ labelStr ++ "\"" ++ " " ++ 
            transitionStr One zWrite oMove ++ "\"" ++ "];",   -- state s see 1
    show s ++ arrowStr ++ stateToStr bState ++ labelStr ++ "\"" ++ " " ++ 
            transitionStr Blank zWrite bMove ++ "\"" ++ "];"   -- state s see blank
    ]


----------------------------------------------------
-- do elem should take in any element and output the 
-- dotfile code for it
----------------------------------------------------
doElem :: Element -> [DotFileLine]
doElem (StateBlock s zAction oAction bAction) = stateCode (StateBlock s zAction oAction bAction)
doElem (TapeBlock _) = noCode


----------------------------------------------------
-- Do elems kind of like in picEvaluation file from pictops hw
-- should take in list of elements and output dotfile code
----------------------------------------------------
doElems :: [Element] -> [DotFileLine]
doElems [] = noCode   -- base case of empty elements list
-- If there are any elements, do recursive call where we call doElem 
--    on one element, doElems on rest
doElems (e:es) = dot1 ++ dot2
        where dot1 = doElem e
              dot2 = doElems es

------------------------------------------------
-- Helper fn to create dot files
------------------------------------------------
emitCode :: String -> [DotFileLine] -> IO ()
emitCode dotFileName dotCode =
    do putStrLn ("Writing " ++ dotFileName)
       writeFile dotFileName (unlines headerCode ++ unlines dotCode ++ unlines trailerCode)
