module TuringEval where

import TM_AST
import Data.Map (Map)
import qualified Data.Map as Map
import System.Process (CreateProcess(env))
import Data.Type.Coercion (sym)


-- | State Environment mapping (currentState, symbol seen) -> (newSym, Move, NewState)
type StateEnv = Map (Integer, Alphabet) (Alphabet, Move, State)

-- The starting (blank) environment for the evaluator
initialEnv :: StateEnv
initialEnv = Map.empty

-- Helper function to return literal tuples corresponding to a state and current symbol
stateLookup :: (Integer, Alphabet ) -> StateEnv -> (Alphabet, Move, State)
stateLookup (n, symRead) env = Map.findWithDefault (Zero, Stay, Halt) (n, symRead) env

-- | Helper function to do move on a tm tape:
-- Takes in: leftTape, rightTape, move
-- Outputs: (newLefttape, newRightTape)
doMove :: Tape -> Tape -> Move -> (Tape, Tape)
doMove [] rightTape Lt = ([], Blank : rightTape)
doMove (leftHead : leftTape) rightTape Lt = (leftTape, leftHead : rightTape)
doMove leftTape [elem] Rt = (elem : leftTape, [Blank])
doMove leftTape (rightHead : rightTape) Rt = (rightHead : leftTape, rightTape) 
doMove leftTape rightTape Stay = (leftTape, rightTape)


-- | Helper function to modify the current symbol on the tape
--   would happen before doing move
doWrite :: Tape -> Alphabet -> Tape
doWrite (rightHead : rightTape) symbol = symbol : rightTape

-- Function to evaluate individual element parsed, which are either a stateblock or a tapeblock
initElem :: Element -> StateEnv -> (Tape, Tape, State, StateEnv)
initElem (StateBlock n a0 a1 ab) env = initStates n a0 a1 ab env
initElem (TapeBlock tp) env = runTape (initTape tp env)

-- Primary function for evaluating the parsed txt file of a turing machine.
-- Uses recursion and leverages the fact that the tape block is always the last element.
runTM :: [Element] -> StateEnv -> (Tape, Tape, State, StateEnv)
runTM [TapeBlock tp] env = initElem (TapeBlock tp) env
runTM (e : es) env = (ltR', rt', state', env'')
                        where (ltR', rt', state', env'') = runTM es env'
                              (_, _, _, env') = initElem e env 


-- Helper function for initElem to add states when a stateblock is seen.
initStates :: Integer -> Action -> Action-> Action -> StateEnv -> (Tape, Tape, State, StateEnv)
initStates n (Action r0 w0 m0 s0) (Action r1 w1 m1 s1) (Action rb wb mb sb) env =
    ([], [], State 0, foldl (\env (k, v) -> Map.insert k v env) env stateList) 
    where stateList = [((n, r0), (w0, m0, s0)), ((n, r1), (w1, m1, s1)), ((n, rb), (wb, mb, sb))]

-- Helper function to initialize a tape so that runTape can run with the full state environment.
initTape :: Tape -> StateEnv -> (Tape, Tape, State, StateEnv)
initTape tp env = (ltTapeReverse, [rtHead], State 0, env)
              where rtHead : ltTapeReverse = reverse tp

-- | Evals one single place and one digit
-- | Takes in (left, right, curr state), state environment, outputs (newL, newR, newState)
runTape :: (Tape, Tape, State, StateEnv) -> (Tape, Tape, State, StateEnv)
runTape (ltReverse, rtTape, Halt, env) = (ltReverse, rtTape, Halt, env)
runTape (ltReverse, symRead : rtRest, State n, env) = runTape (ltTapeNext, rtTapeNext, newState, env)
          where (symWritten, move, newState) = stateLookup (n, symRead) env
                rtTape' = doWrite (symRead : rtRest) symWritten
                (ltTapeNext, rtTapeNext) = doMove ltReverse rtTape' move


-- | Convert abstract syntax representing a tape to a string of literal 
-- 1s, 0s, and _s for final output IO printing (helper for returnTape)
convertBackTape :: Tape -> String
convertBackTape [] = ""
convertBackTape (Zero : rest) = '0' : convertBackTape rest
convertBackTape (One : rest) = '1' : convertBackTape rest
convertBackTape (Blank : rest) = '_' : convertBackTape rest

-- | Takes in abstract syntax representations of the left tape reverse representation
--    and right tape, and merges them into one string using concatenation
--    and convert helper. Also for output printing--the overall function
returnTape :: (Tape, Tape) -> String
returnTape (ltReverse, rtTape) = convertBackTape (reverse ltReverse ++ rtTape)