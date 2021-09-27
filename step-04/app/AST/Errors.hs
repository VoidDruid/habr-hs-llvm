module AST.Errors where

class GError e where -- TODO: error type
    showE :: e -> String

newtype GTypeError = GTypeError String
  deriving Show

instance GError GTypeError where
  showE (GTypeError err) = "Type Error: <" ++ err ++ ">"