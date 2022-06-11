module RpgLisp.Grammar where

data ParsedExpr = PExprInt Int
                | PExprString String
                | PExprBool Bool
                | PExprNil
                | PExprAtom String
                | PExprList [ParsedExpr]
                deriving (Eq, Show)

data FuncData m = Func [(String, Expr m)] [String] [Expr m]
                | SpecialFunc [(String, Expr m)] [String] [Expr m]
                | PrimitiveFunc ([Expr m] -> m (Expr m))

data Expr m = ExprInt Int
            | ExprString String
            | ExprBool Bool
            | ExprNil
            | ExprAtom String
            | ExprList [Expr m]
            | ExprFunc (FuncData m)

instance Show (Expr m) where
  show (ExprInt value) = show value
  show (ExprString value) = "\"" ++ value ++ "\""
  show (ExprBool value) = if value then "#t" else "#f"
  show ExprNil = "#nil"
  show (ExprAtom atom) = "(Atom " ++ atom ++ ")"
  show (ExprList values) = "(" ++ unwords (show <$> values) ++ ")"
  show (ExprFunc _) = "<FUNC>"

convertParsed :: (Monad m) => ParsedExpr -> Expr m
convertParsed (PExprInt value) = ExprInt value
convertParsed (PExprString value) = ExprString value
convertParsed (PExprBool value) = ExprBool value
convertParsed PExprNil = ExprNil
convertParsed (PExprAtom value) = ExprAtom value
convertParsed (PExprList values) = ExprList (convertParsed <$> values)

extractInt :: Expr m -> Maybe Int
extractInt (ExprInt value) = Just value
extractInt _ = Nothing

extractString :: Expr m -> Maybe String
extractString (ExprString value) = Just value
extractString _ = Nothing

extractBool :: Expr m -> Maybe Bool
extractBool (ExprBool value) = Just value
extractBool _ = Nothing

extractList :: Expr m -> Maybe [Expr m]
extractList (ExprList values) = Just values
extractList _ = Nothing

extractAtom :: Expr m -> Maybe String
extractAtom (ExprAtom atom) = Just atom
extractAtom _ = Nothing
