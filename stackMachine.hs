import Data.Map (Map)
import qualified Data.Map as Map
import Text.Read (readMaybe)

data MachineCommand = Add
    |Mul
    |Sub
    |Div
    |PushVar String
    |PushConst Double 
    deriving(Eq)

data MachineState = MachineState [Double] --(Map String Double)
    deriving (Show) 

getStack :: MachineState -> [Double]
getStack (MachineState stack) = stack

-- getVarTable :: MachineState -> Map String Double
-- getVarTable (MachineState _ table) = table

varMap :: Map String Double
varMap = Map.fromList
  [ ("x", 3.0),
    ("y", 5.0)
  ]

runCommand::  Either String MachineState -> MachineCommand -> Either String MachineState
runCommand (Right (MachineState stack)) (PushConst x)  = Right $ MachineState (x:stack)
runCommand (Right (MachineState stack)) (PushVar name) =
    case Map.lookup name varMap of
        Just val -> Right $ MachineState (val:stack)
        Nothing -> Left $ "Variable '" ++ name ++ "' not found"
runCommand (Right state) Add = binaryOp (+) state
runCommand (Right state) Mul = binaryOp (*) state
runCommand (Right state) Sub = binaryOp (-) state
runCommand (Right (MachineState stack)) Div  = 
    case stack of
        x:y:xs -> if x == 0 
                  then Left "Division by zero" 
                  else Right $ MachineState ((y / x) : xs)
        _ -> Left "Not enough operands for division"

runCommand (Left error) _ = Left error

binaryOp :: (Double -> Double -> Double) -> MachineState -> Either String MachineState
binaryOp op (MachineState stack) = 
    case stack of
        x:y:xs -> Right $ MachineState (op y x : xs)
        _ -> Left "Not enough operands on stack"

runProgram :: [MachineCommand] -> Either String MachineState -> Either String MachineState

runProgram program initialState = foldl runCommand initialState program

parseCommand :: String -> MachineCommand
parseCommand str = 
    case words str of
        ["push", arg] -> 
            case readMaybe arg of
                Just num -> PushConst num
                Nothing -> PushVar arg
        ["add"] -> Add
        ["mul"] -> Mul
        ["sub"] -> Sub
        ["div"] -> Div

parseProgram :: [String] -> [MachineCommand]
parseProgram = map parseCommand

initialState :: MachineState
initialState = MachineState []

example = 
    let program = parseProgram ["push 5","push 4","push x","mul","push 2","push x","push x","mul","mul","add","add"]
    in runProgram program (Right initialState)
