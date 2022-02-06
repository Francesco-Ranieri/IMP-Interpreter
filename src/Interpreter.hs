module Interpreter where

import Dictionary (Dictionary, empty, get, insert, delete)
import Grammar ( Command(..), BExp(..), AExp(..), Type(..), ArrayExp(..))
import Array (Array,declare, read, write)

type State = Dictionary String Type

emptyState :: State
emptyState = empty

aExpEval :: State -> AExp -> Maybe Int
aExpEval _ (AExpConstant c) = Just c
aExpEval s (AExpVariable v) = 
  case get s v of
    Just (IntegerType r) -> Just r 
    Just (BooleanType _) -> error "Variable of type boolean!"
    Just (ArrayType _) -> error "Variable of type array!"
    Nothing -> error "Variable not found!"
aExpEval s (ValueFromArray v i) =
  case get s v of
    Just (IntegerType _) -> error "Variable of type integer!"
    Just (BooleanType _) -> error "Variable of type boolean!"
    Just (ArrayType r) -> Array.read i' r
                            where Just i' = aExpEval s i
    Nothing -> error "Variable not found!"
aExpEval s (Add a b) = (+) <$> aExpEval s a <*> aExpEval s b
aExpEval s (Sub a b) = (-) <$> aExpEval s a <*> aExpEval s b
aExpEval s (Mul a b) = (*) <$> aExpEval s a <*> aExpEval s b

bExpEval :: State -> BExp -> Maybe Bool
bExpEval _ (BExpConstant b) = Just b
bExpEval s (BExpVariable v) = 
  case get s v of
    Just (IntegerType _) -> error "Variable of type integer!"
    Just (BooleanType r) -> Just r
    Just (ArrayType _) -> error "Variable of type array!"
    Nothing -> error "Variable not found"
bExpEval s (Not b) = not <$> bExpEval s b
bExpEval s (Or a b) = (||) <$> bExpEval s a <*> bExpEval s b
bExpEval s (And a b) = (&&) <$> bExpEval s a <*> bExpEval s b
bExpEval s (Less a b) = (<) <$> aExpEval s a <*> aExpEval s b
bExpEval s (LessEqual a b) = (<=) <$> aExpEval s a <*> aExpEval s b
bExpEval s (Greater a b) = (>) <$> aExpEval s a <*> aExpEval s b
bExpEval s (GreaterEqual a b) = (>=) <$> aExpEval s a <*> aExpEval s b
bExpEval s (Equal a b) = (==) <$> aExpEval s a <*> aExpEval s b
bExpEval s (NotEqual a b) = (/=) <$> aExpEval s a <*> aExpEval s b

arrayExpEval :: State -> ArrayExp -> Maybe (Array Int)
arrayExpEval s (ArrayValues a) = if hasFailed 
                                    then Nothing
                                    else Just $ map (\v -> case v of Just x -> x) r
                                      where hasFailed = or $ map (\v -> case v of
                                              Nothing -> True
                                              Just x -> False) r
                                            r = map (\exp -> aExpEval s exp) a
arrayExpEval s (ArrayExpVariable v) = 
  case get s v of
    Just (IntegerType _) -> error "Assignment of an integer value to an array one not allowed!"
    Just (BooleanType _) -> error "Assignment of an boolean value to an array one not allowed!"
    Just (ArrayType a) -> Just a
    Nothing -> error "Variable to assign not found"

executeCommands :: State -> [Command] -> State
executeCommands s [] = s
executeCommands s (Skip : cs) = executeCommands s cs


                                                -- START DECLARATION AREA --

-- EXECUTE INTEGER DECLARATION --
executeCommands s ((AExpDeclaration v exp) : cs) =
  case aExpEval s exp of
    Just ex' -> case get s v of
                  Just _ -> error "Variable already declared!"
                  Nothing -> executeCommands (insert s v (IntegerType ex')) cs
    Nothing -> error "Invalid aExp"


-- EXECUTE BOOLEAN DECLARATION --
executeCommands s ((BExpDeclaration v exp) : cs) =
  case bExpEval s exp of
    Just ex' -> case get s v of
                  Just _ -> error "Variable already declared!"
                  Nothing -> executeCommands (insert s v (BooleanType ex')) cs
    Nothing -> error "Invalid bExp"


-- EXECUTE ARRAY DECLARATION --
executeCommands s ((ArrayDeclaration v exp) : cs) =
  case aExpEval s exp of
    Just ex' -> case get s v of
                  Just _ -> error "Variable already declared!"
                  Nothing -> executeCommands (insert s v (ArrayType a)) cs
                              where a = declare ex'
    Nothing -> error "Invalid size!"


                                                -- START ASSIGMENT AREA --

-- EXECUTE INTEGER ASSIGMENT --
executeCommands s ((AExpAssignment v exp) : cs) =
  case get s v of
    Just (IntegerType _) -> executeCommands (insert s v (IntegerType exp')) cs
                              where Just exp' = aExpEval s exp
    Just (BooleanType _) -> error "Assignment of a boolean value to an aExp variable not allowed!"
    Just (ArrayType _) -> error "Assignment of an array value to an aExp variable not allowed!"
    Nothing -> error "Undeclared variable!"


-- EXECUTE BOOLEAN ASSIGMENT --
executeCommands s ((BExpAssignment v exp) : cs) =
  case get s v of
    Just (BooleanType _) -> executeCommands (insert s v (BooleanType exp')) cs
                              where Just exp' = bExpEval s exp
    Just (IntegerType _) -> error "Assignment of an integer value to a bExp variable not allowed!"
    Just (ArrayType _) -> error "Assignment of an array value to an bExp variable not allowed!"
    Nothing -> error "Undeclared variable!"


-- EXECUTE ARRAY ASSIGMENT SINGLE VALUE --
executeCommands s ((ArrayAssignmentSingleValue v i exp) : cs) =
  case get s v of
    Just (ArrayType a) -> case aExpEval s exp of
                            Just r -> executeCommands (insert s v (ArrayType exp')) cs
                              where Just exp' = Array.write i' r a
                                                  where Just i' = aExpEval s i
                            Nothing -> error "The expression you want to assign is not valid!"
    Just (IntegerType _) -> error "Assignment of an integer value to an array variable not allowed!"
    Just (BooleanType _) -> error "Assignment of an boolean value to an array variable not allowed!"
    Nothing -> error "Undeclared variable!"

-- EXECUTE ARRAY ASSIGMENT MULTI VALUE --
executeCommands s ((ArrayAssignmentValues v exp) : cs) =
  case get s v of
    Just (ArrayType a) -> case arrayExpEval s exp of
                            Just b -> if length a == length b 
                                        then executeCommands (insert s v (ArrayType b)) cs
                                        else error "Length not valid!"
                            Nothing -> error "One of the aExp evaluation of the array you want to assign failed"
    Just (IntegerType _) -> error "Assignment of an aExp value to an array variable not allowed!"
    Just (BooleanType _) -> error "Assignment of an bExp value to an array variable not allowed!"
    Nothing -> error "Undeclared variable!"


                                               -- START CONTROL CONSTRUCTS AREA --

-- EXECUTE IfThenElse COMMAND --
executeCommands s ((IfThenElse b c c') : cs) =
  case bExpEval s b of
    Just True -> executeCommands s (c ++ cs)
    Just False -> executeCommands s (c' ++ cs)
    Nothing -> error "Invalid boolean expression!"

-- EXECUTE WHILE COMMAND --
executeCommands s ((While b c) : cs) =
  case bExpEval s b of
    Just True -> executeCommands s (c ++ [While b c] ++ cs)
    Just False -> executeCommands s cs
    Nothing -> error "Invalid boolean expression!"

-- EXECUTE FOR WITH INCREMENT COMMAND --
executeCommands s ((ForIncrement counter booleanExp identifier body) : cs) =
  let c = body ++ [AExpAssignment identifier (Add (AExpVariable identifier) (AExpConstant 1))]
  in
    executeCommands s ([counter] ++ [(While booleanExp c)] ++ cs)

-- EXECUTE FOR WITH DECREMENT COMMAND --
executeCommands s ((ForDecrement counter booleanExp identifier body) : cs) =
  let c = body ++ [AExpAssignment identifier (Sub (AExpVariable identifier) (AExpConstant 1))]
  in
    executeCommands s ([counter] ++ [(While booleanExp c)] ++ cs)