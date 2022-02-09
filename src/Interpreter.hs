module Interpreter where

import Dictionary (Dictionary, empty, get, insert, delete)
import Grammar ( Command(..), BExp(..), AExp(..), Type(..), ArrayExp(..), SetExp(..), StackExp(..))
import Array (Array,declare, read, write)
import Set (Set,declareSet, readSet, insertSet, arrayToSet, fullDeclareSet)
import Stack(Stack, declareStack, pushValue, popValue)
import Utils(isEmpty, len)

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
    Just (SetType _) -> error "Variable of type set!"
    Nothing -> error "Variable not found!"
aExpEval s (ValueFromArray v i) =
  case get s v of
    Just (IntegerType _) -> error "Variable of type integer!"
    Just (BooleanType _) -> error "Variable of type boolean!"
    Just (SetType _) -> error "Variable of type set!"
    Just (ArrayType r) -> Array.read i' r
                            where Just i' = aExpEval s i
    Nothing -> error "Variable not found!"
aExpEval s (Add a b) = (+) <$> aExpEval s a <*> aExpEval s b
aExpEval s (Sub a b) = (-) <$> aExpEval s a <*> aExpEval s b
aExpEval s (Mul a b) = (*) <$> aExpEval s a <*> aExpEval s b
aExpEval s (Length i) =
    case get s i of
        Just (ArrayType a) -> Just (Utils.len a)
        Just (SetType a) -> Just (Utils.len a)
        Just (StackType a) -> Just (Utils.len a)
        Just _ -> error "Function LEN not defined for this type of variable!"
        Nothing -> error "Variable not found"

bExpEval :: State -> BExp -> Maybe Bool
bExpEval _ (BExpConstant b) = Just b
bExpEval s (BExpVariable v) = 
  case get s v of
    Just (IntegerType _) -> error "Variable of type integer!"
    Just (BooleanType r) -> Just r
    Just (ArrayType _) -> error "Variable of type array!"
    Just (SetType _) -> error "Variable of type set!"
    Just (StackType _) -> error "Variable of type stack!"
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
bExpEval s (IsEmpty i) =
    case get s i of
      Just (SetType a) -> Just (Utils.isEmpty a)
      Just (ArrayType a) -> Just (Utils.isEmpty a)
      Just (StackType a) -> Just (Utils.isEmpty a)
      Just _ -> error "Cannot apply function to variable"
      Nothing -> Nothing

-- ARRAY EVAL
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
    Just (SetType _) -> error "Assignment of a set value to an array one not allowed!"
    Just (ArrayType a) -> Just a
    Nothing -> error "Variable to assign not found"


--SET EVAL
setExpEval :: State -> SetExp -> Maybe (Set Int)
setExpEval s (SetValues a) = if hasFailed
                                    then Nothing
                                    else Just $ map (\v -> case v of Just x -> x) r
                                      where hasFailed = or $ map (\v -> case v of
                                              Nothing -> True
                                              Just x -> False) r
                                            r = map (\exp -> aExpEval s exp) a


setExpEval s (SetExpVariable v) =
  case get s v of
    Just (IntegerType _) -> error "Assignment of an integer value to a set one not allowed!"
    Just (BooleanType _) -> error "Assignment of an boolean value to a set one not allowed!"
    Just (ArrayType a) -> Just (Set.fullDeclareSet a)
    Just (SetType b) -> Just b
    Nothing -> error "Variable to assign not found"


-- STACK EVAL
stackExpEval :: State -> StackExp -> Maybe (Stack Int)
stackExpEval s (StackValues a) = if hasFailed
                                    then Nothing
                                    else Just $ map (\v -> case v of Just x -> x) r
                                      where hasFailed = or $ map (\v -> case v of
                                              Nothing -> True
                                              Just x -> False) r
                                            r = map (\exp -> aExpEval s exp) a


stackExpEval s (StackExpVariable v) =
  case get s v of
    Just (ArrayType a) -> Just (Set.fullDeclareSet a)
    Just (SetType b) -> Just b
    Just _ -> error "Assignment not allowed!"
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


executeCommands s ((ArrayFullDeclaration v exp) : cs) =
    case get s v of
     Just _ -> error "Variable already declared!"
     Nothing -> case arrayExpEval s exp of
                    Just b -> executeCommands (insert s v (ArrayType b)) cs


-- EXECUTE SET DECLARATION --
executeCommands s ((SetDeclaration v) : cs) =
    case get s v of
      Just _ -> error "Variable already declared!"
      Nothing -> executeCommands (insert s v (SetType []) ) cs


executeCommands s ((SetFullDeclaration v exp) : cs) =
    case get s v of
     Just _ -> error "Variable already declared!"
     Nothing -> case setExpEval s exp of
                    Just b -> executeCommands (insert s v (SetType c)) cs
                        where
                         c = Set.arrayToSet b

-- EXECUTE STACK DECLARATION

executeCommands s ((StackDeclaration i) : cs) =
    case get s i of
        Just _ -> error "Variable already declared!"
        Nothing -> executeCommands (insert s i (StackType emptyStack)) cs
                    where
                     emptyStack = Stack.declareStack

                                                -- START ASSIGMENT AREA --

-- EXECUTE INTEGER ASSIGMENT --
executeCommands s ((AExpAssignment v exp) : cs) =
  case get s v of
    Just (IntegerType _) -> executeCommands (insert s v (IntegerType exp')) cs
                              where Just exp' = aExpEval s exp
    Just (BooleanType _) -> error "Assignment of a boolean value to an aExp variable not allowed!"
    Just (ArrayType _) -> error "Assignment of an array value to an aExp variable not allowed!"
    Just (SetType _) -> error "Assignment of a set value to an aExp variable not allowed!"
    Nothing -> error "Undeclared variable!"


-- EXECUTE BOOLEAN ASSIGMENT --
executeCommands s ((BExpAssignment v exp) : cs) =
  case get s v of
    Just (BooleanType _) -> executeCommands (insert s v (BooleanType exp')) cs
                              where Just exp' = bExpEval s exp
    Just (IntegerType _) -> error "Assignment of an integer value to a bExp variable not allowed!"
    Just (ArrayType _) -> error "Assignment of an array value to an bExp variable not allowed!"
    Just (SetType _) -> error "Assignment of a set value to an bExp variable not allowed!"
    Nothing -> error "Undeclared variable!"


-- EXECUTE ARRAY ASSIGMENT SINGLE VALUE --
executeCommands s ((ArrayAssignmentSingleValue v i exp) : cs) =
  case get s v of
    Just (ArrayType a) -> case aExpEval s exp of
                            Just r -> executeCommands (insert s v (ArrayType exp')) cs
                              where Just exp' = Array.write i' r a
                                                  where Just i' = aExpEval s i
                            Nothing -> error "The expression you want to assign is not valid!"
    Just (SetType a) -> error "Cannot access a specific position of a set!"
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
    Just (SetType _) -> case arrayExpEval s exp of
                            Just b -> executeCommands (insert s v (SetType c)) cs
                                where
                                 c = Set.arrayToSet b
                            Nothing -> error "One of the aExp evaluation of the array you want to assign failed"
    Just (IntegerType _) -> error "Assignment of an aExp value to an array variable not allowed!"
    Just (BooleanType _) -> error "Assignment of an bExp value to an array variable not allowed!"
    Nothing -> error "Undeclared variable!"


-- EXECUTE SET ASSIGMENT SINGLE VALUE --
executeCommands s ((SetAssignmentSingleValue set exp) : cs) =
  case get s set of
    Just (SetType a) -> case aExpEval s exp of
                            Just r -> executeCommands (insert s set (SetType exp1)) cs
                              where exp1 = Set.insertSet r a
                            Nothing -> error "The expression you want to assign is not valid!"
    Just _ -> error "Cannot Add!"
    Nothing -> error "Undeclared variable!"


-- EXECUTE SET ASSIGMENT MULTI VALUE --
executeCommands s ((SetAssignmentValues v exp) : cs) =
  case get s v of
    Just (SetType a) -> case setExpEval s exp of
                            Just b -> executeCommands (insert s v (SetType c)) cs
                                where
                                 c = Set.arrayToSet b
                            Nothing -> error "The expression you want to assign is not valid!"
    Just (ArrayType a) -> case setExpEval s exp of
                            Just b -> executeCommands (insert s v (ArrayType c)) cs
                                where
                                 c = Set.arrayToSet b
                            Nothing -> error "The expression you want to assign is not valid!"
    Just _ -> error "Assignment not allowed!"
    Nothing -> error "Undeclared variable!"

                                                          -- STACK --

executeCommands s ((StackPushValue v exp) : cs) =
  case get s v of
    Just (StackType a) -> case aExpEval s exp of
                              Just b -> executeCommands (insert s v (StackType c)) cs
                                where
                                 c = Stack.pushValue a b
                              Nothing -> error "The expression you want to assign is not valid!"
    Just _ -> error "Assignment not allowed!"
    Nothing -> error "Undeclared variable!"


executeCommands s ((StackPopValue v) : cs) =
  case get s v of
    Just (StackType a) -> executeCommands (insert s v (StackType c)) cs
                            where
                             c = Stack.popValue a
    Just _ -> error "Assignment not allowed!"
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