module TM_AST where


type TuringMachine = [Element]

-- Element that we may encounter while parsing:
-- Stateblock of state number, action0 action1 actionBlank
-- or Tapeblock: init tape representation
data Element = StateBlock Integer Action Action Action 
             | TapeBlock Tape
             deriving (Show, Eq, Ord)

-- | define data structure for alphabet
data Alphabet = Zero | One | Blank
    deriving (Show, Eq, Ord)

-- | Define data structure for movements
data Move = Lt | Rt | Stay 
    deriving (Show, Eq, Ord)

-- | Defining the stack datatype 
type Tape = [Alphabet]

-- | Defining the data structure of state representation
data State = State Integer | Halt
    deriving (Show, Eq, Ord)

-- | Action upon seeing a symbol: (symbol read, symbol written, move, next state)
data Action = Action Alphabet Alphabet Move State
    deriving (Show, Eq, Ord)