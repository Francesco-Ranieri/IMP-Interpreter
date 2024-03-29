module Grammar where

import Array

data Type 
    --
    = IntegerType Int
    -- 
    | BooleanType Bool
    --
    | ArrayType (Array Int)
    --
    | SetType (Array Int)
    --
    | StackType (Array Int)
    deriving Show

data AExp
    -- Constant integer
    = AExpConstant Int
    -- Identifier string
    | AExpVariable String
    -- value contained in a specific position of an array variable
    | ValueFromArray String AExp
    -- Addition between sub-expressions
    | Add AExp AExp
    -- Subtraction between sub-expressions
    | Sub AExp AExp
    -- Multiplication between sub-expressions
    | Mul AExp AExp
    --
    | Length String
    deriving Show

data BExp
    -- Ground True and False
    = BExpConstant Bool
    -- Identifier string
    | BExpVariable String
    -- Not unary operator
    | Not BExp
    -- Or binary operator
    | Or BExp BExp
    -- And binary operator
    | And BExp BExp
    -- < binary operator between arithmetical expressions
    | Less AExp AExp
    -- > binary operator between arithmetical expressions
    | Greater AExp AExp
    -- >= binary operator between arithmetical expressions
    | LessEqual AExp AExp
    -- <= binary operator between arithmetical expressions
    | GreaterEqual AExp AExp
    -- == binary operator between arithmetical expressions
    | Equal AExp AExp
    -- != binary operator between arithmetical expressions
    | NotEqual AExp AExp
    -- is_empty operator --> True is structure is empty
    | IsEmpty String
    deriving Show

data ArrayExp
    --  x = [1,2,3,3]
    = ArrayValues [AExp]
    -- x = y       y = [1,2,3,3]
    | ArrayExpVariable String
    deriving Show

data SetExp
    --  x = [1,2,3]
    = SetValues [AExp]
    -- x = y       y = [1,2,3]
    | SetExpVariable String
    deriving Show

data StackExp
    --  x = [1,2,3]
    = StackValues [AExp]
    -- x = y       y = [1,2,3]
    | StackExpVariable String
    deriving Show

-- Command declaration
data Command
    --
    = AExpDeclaration String AExp
    --
    | BExpDeclaration String BExp
    -- Array
    | ArrayDeclaration String AExp
    --
    | ArrayFullDeclaration String ArrayExp
    -- Set
    | SetDeclaration String
    --
    | SetFullDeclaration String SetExp
    -- Stack
    | StackDeclaration String
    --
    | Skip
    --
    | AExpAssignment String AExp
    --
    | BExpAssignment String BExp
    --
    | ArrayAssignmentSingleValue String AExp AExp
    --
    | ArrayAssignmentValues String ArrayExp
    --
    | SetAssignmentSingleValue String AExp
    --
    | SetAssignmentValues String SetExp
    --
    | StackPushValue String AExp
    --
    | StackPopValue String
    --
    | IfThenElse BExp [Command] [Command]
    --
    | While BExp [Command]
    --
    | ForIncrement Command BExp String [Command]
    --
    | ForDecrement Command BExp String [Command]
    deriving Show

-- Program declaration
type Program = [Command]
