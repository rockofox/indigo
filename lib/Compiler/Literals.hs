module Compiler.Literals where

import AST qualified as Parser
import Compiler.State (Compiler)
import VM (Data (..), Instruction (..))

{- | Compile literal expressions.
Returns 'Just' instructions if the expression is a literal, 'Nothing' otherwise.
-}
compileLiterals :: Parser.Expr -> Parser.Type -> Compiler (Maybe [Instruction])
compileLiterals expr _ = case expr of
    Parser.IntLit{intValue = x} -> return $ Just [Push $ DInt $ fromIntegral x]
    Parser.StringLit{stringValue = x} -> return $ Just [Push $ DString x]
    Parser.FloatLit{floatValue} -> return $ Just [Push $ DFloat floatValue]
    Parser.DoubleLit{doubleValue} -> return $ Just [Push $ DDouble doubleValue]
    Parser.BoolLit{boolValue} -> return $ Just [Push $ DBool boolValue]
    Parser.CharLit{charValue = x} -> return $ Just [Push $ DChar x]
    Parser.UnaryMinus{unaryMinusExpr = Parser.FloatLit{floatValue = x}} -> return $ Just [Push $ DFloat (-x)]
    Parser.UnaryMinus{unaryMinusExpr = Parser.IntLit{intValue = x}} -> return $ Just [Push $ DInt $ -fromInteger x]
    _ -> return Nothing
