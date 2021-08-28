module Py2Hs where

import Data.List
import Data.Maybe (fromMaybe)
import Data.Traversable (for)
import Language.Python.Common

data Purity = Pure | Impure
  deriving (Eq, Show)

-- type Annot = (Purity, SrcSpan)
type Annot = SrcSpan

-- TODO: change from String to Text
-- TODO: move generated imports and helper functions to another module, then just import it

transpileParsed :: (ModuleSpan, [Token]) -> String
transpileParsed (mod, _) = transpileModule mod

transpileModule :: Module Annot -> String
transpileModule (Module statements) =
  -- "{-# LANGUAGE OverloadedRecordDot #-}\n\n" ++

  "module BinarySearch where\n\n" ++
  "import Data.Bits\n" ++
  "import Data.Map (Map)\n" ++
  "import qualified Data.Map as Map\n" ++
  "import Data.Set (Set)\n" ++
  "import qualified Data.Set as Set\n" ++
  "\n" ++

  "range :: Enum a => a -> a -> [a]\n" ++
  "range m n = [m..n]\n" ++
  "\n" ++

  "-- https://stackoverflow.com/questions/4597820/does-haskell-have-list-slices-i-e-python\n" ++
  "takeStep :: Int -> [a] -> [a]\n" ++
  "takeStep _ [] = []\n" ++
  "takeStep n (x:xs)\n" ++
  "  | n >= 0 = x : takeStep n (drop (n-1) xs)\n" ++
  "  | otherwise = takeStep (-n) (reverse xs)\n\n" ++
  "slice :: Int -> Int -> Int -> [a] -> [a]\n" ++
  "slice a e d xs = z . y . x $ xs -- a:start, e:stop, d:step\n" ++
  "  where a' = if a >= 0 then a else (length xs + a)\n" ++
  "        e' = if e >= 0 then e else (length xs + e)\n" ++
  "        x = if d >= 0 then drop a' else drop e'\n" ++
  "        y = if d >= 0 then take (e'-a') else take (a'-e'+1)\n" ++
  "        z = takeStep d\n" ++
  "\n" ++

  (intercalate "\n\n" $ map transpileStatement statements)

transpileStatement :: Statement Annot -> String
transpileStatement (Import items annot) = unsupported "import" annot
transpileStatement (FromImport modul items annot) = unsupported "from import" annot
transpileStatement (While condition body els annot) = unsupported "while" annot
transpileStatement (For targets generator body els annot) = unsupported "for" annot
transpileStatement (AsyncFor stmt annot) = unsupported "async for" annot
transpileStatement (Fun (Ident name _) args resultAnnotation body statementAnnotation) =
  concat
    [ name
    , " "
    , intercalate " " $ map transpileParameter args
    , " =\n  "
    ] ++ concatMap transpileStatement body
transpileStatement (Class name args body annot) = unsupported "class" annot
transpileStatement (Conditional guards els _) = transpileGuards guards els
transpileStatement (Assign names value _) =
  let f expr = "let " ++ transpileExpr expr ++ " = " ++ transpileExpr value ++ " in\n  "
   in intercalate "\n" $ map f names

transpileStatement (AugmentedAssign to op expr annot) = unsupported "augmentedAssign" annot
transpileStatement (AnnotatedAssign annotation to expr annot) = unsupported "annotatedAssign" annot
transpileStatement (Decorated decorators def annot) = unsupported "decorator" annot
transpileStatement (Return mExpr _) = case mExpr of
  Nothing -> ""
  Just expr -> transpileExpr expr
transpileStatement (Try body excepts els finally annot) = unsupported "try" annot
transpileStatement (Raise expr annot) = unsupported "raise" annot
transpileStatement (With context body annot) = unsupported "with" annot
transpileStatement (AsyncWith stmt annot) = unsupported "asyncWith" annot
transpileStatement (Break annot) = unsupported "break" annot
transpileStatement (Continue annot) = unsupported "continue" annot
transpileStatement (Delete expr annot) = unsupported "del" annot
transpileStatement (StmtExpr expr annot) = transpileExpr expr
transpileStatement (Global vars annot) = unsupported "global" annot
transpileStatement (NonLocal vars annot) = unsupported "nonlocal" annot
transpileStatement (Assert exprs annot) = unsupported "assert" annot
transpileStatement (Print chevron exprs hasTrailingComma annot) = unsupported "print" annot
transpileStatement (Exec expr globalsLocals annot) = unsupported "exec" annot

transpileParameter :: Parameter Annot -> String
transpileParameter (Param (Ident name _) mAnnotation defaultValue _) = name
transpileParameter (VarArgsPos (Ident name _) mAnnotation _) = name
transpileParameter (VarArgsKeyword (Ident name _) mAnnotation _) = name
transpileParameter (EndPositional _) = ""
transpileParameter (UnPackTuple tuple defaultValue _) = ""

transpileParamTuple :: ParamTuple Annot -> String
transpileParamTuple (ParamTupleName (Ident name _) _) = name
transpileParamTuple (ParamTuple paramTuples _) =
  intercalate " " $ map transpileParamTuple paramTuples

transpileHandler :: Handler Annot -> String
transpileHandler (Handler clause suite annot) =
  unsupported "exception handler" annot

transpileExceptClause :: ExceptClause Annot -> String
transpileExceptClause (ExceptClause mClause annot) =
  unsupported "except" annot

-- transpileRaiseExpr :: RaiseExpr Annot -> String
-- transpileRaiseExpr r@(RaiseV3 _) = unsupported "raise" $ annot r
-- transpileRaiseExpr r@(RaiseV2 _) = unsupported "raise" $ annot r

transpileGuards :: [(Expr Annot, Suite Annot)] -> Suite Annot -> String
transpileGuards [] _ = ""
transpileGuards [(expr, statements)] els =
  concat
    [ "if "
    , transpileExpr expr
    , "\n  then "
    , concatMap transpileStatement statements
    , "\n  else "
    , if not $ null els then concatMap transpileStatement els else ""
    ]

parenWrap s =
  if ' ' `elem` s && not (firstLastParen s)
  then "(" ++ s ++ ")"
  else s
  where firstLastParen s = head s == '(' && last s == ')'

transpileExpr :: Expr Annot -> String
transpileExpr (Var (Ident name _) _) = name
transpileExpr (Int value literal _) = literal
transpileExpr (LongInt value literal _) = literal
transpileExpr (Float value literal _) = literal
transpileExpr (Imaginary value literal annot) = unsupported literal annot
transpileExpr (Bool value _) = show value
transpileExpr (None _) = "Nothing"
transpileExpr (Ellipsis annot) = unsupported "..." annot
transpileExpr (ByteStrings strings _) = concat strings
transpileExpr (Strings strings _) = map (\c -> if c == '\'' then '"' else c) $ concat strings
transpileExpr (UnicodeStrings strings _) = concat strings
transpileExpr (Call callable args _) =
  concat
    [ transpileExpr callable
    , " "
    , intercalate " " $ map parenWrap $ map transpileArgument args
    ]
transpileExpr (Subscript subscriptee subscript _) =
  transpileExpr subscriptee ++ " !! " ++ transpileExpr subscript
transpileExpr (SlicedExpr slicee slices annot) =
  concatMap transpileSlice slices ++ transpileExpr slicee
transpileExpr (CondExpr trueBranch condition falseBranch annot) = unsupported "condition" annot
transpileExpr (BinaryOp op left right _) =
  concat
    [ transpileExpr left
    , " "
    , transpileOp op
    , " "
    , transpileExpr right
    ]
transpileExpr (UnaryOp op arg _) = transpileOp op ++ transpileExpr arg
transpileExpr (Dot expr (Ident attr _) _) = transpileExpr expr ++ "." ++ attr
transpileExpr (Lambda args body _) =
  concat
    [ "\\"
    , intercalate " " $ map transpileParameter args
    , " -> "
    , transpileExpr body
    ]
transpileExpr (Tuple exprs _) =
  parenWrap $ intercalate ", " $ map transpileExpr exprs
transpileExpr (Yield yield annot) = unsupported "yield" annot
transpileExpr (Generator comprehension annot) = transpileComprehension comprehension
transpileExpr (Await expr annot) = unsupported "await" annot
transpileExpr (ListComp comprehension _) = transpileComprehension comprehension
transpileExpr (List exprs _) =
  concat
    [ "["
    , intercalate ", " $ map transpileExpr exprs
    , "]"
    ]
transpileExpr (Dictionary exprs annot) = concatMap transpileDictKeyDatumList exprs
transpileExpr (DictComp comprehension _) = transpileComprehension comprehension
transpileExpr (Set exprs _) =
  concat
    [ "Set.fromList ["
    , intercalate ", " $ map transpileExpr exprs
    , "]"
    ]
transpileExpr (SetComp comprehension _) = "Set.fromList " ++ transpileComprehension comprehension
transpileExpr (Starred exprs annot) = unsupported "starred" annot
transpileExpr (Paren expr annot) =
  parenWrap $ transpileExpr expr
transpileExpr (StringConversion exprs annot) = unsupported "stringConversion" annot

transpileSlice :: Slice Annot -> String
transpileSlice (SliceProper lower upper stride annot) =
  let low = fromMaybe "0" (transpileExpr <$> lower)
      high = fromMaybe "0" (transpileExpr <$> upper)
      step = case stride of
        Nothing -> "1"
        Just mStride -> fromMaybe "1" (transpileExpr <$> mStride)
  in concat
    [ "slice "
    , show low
    , " "
    , show high
    , " "
    , show stride
    ]
transpileSlice (SliceExpr expr annot) = "!! " ++ transpileExpr expr
transpileSlice (SliceEllipsis annot) = unsupported "..." annot

transpileDictKeyDatumList :: DictKeyDatumList Annot -> String
transpileDictKeyDatumList = error "DictKeyDatumList is not supported"
-- transpileDictKeyDatumList d@(DictMappingPair expr1 expr2) =
--   unsupported "dictKeyDatumListMappingPair" $ annot d
-- transpileDictKeyDatumList d@(DictUnpacking expr) =
--   unsupported "dictKeyDatumListUnpacking" $ annot d

transpileYieldArg :: YieldArg Annot -> String
transpileYieldArg (YieldFrom expr _) = transpileExpr expr
transpileYieldArg (YieldExpr expr) = transpileExpr expr

transpileImportItem :: ImportItem Annot -> String
transpileImportItem (ImportItem dottedName asName annot) =
  unsupported "import item" annot

transpileComprehension :: Comprehension Annot -> String
transpileComprehension (Comprehension (ComprehensionExpr expr) for _) =
  concat
    [ "[ "
    , transpileExpr expr
    , " | "
    , intercalate ", " $ transpileCompFor for
    , " ]"
    ]
transpileComprehension (Comprehension (ComprehensionDict _) for annot) =
  unsupported "ComprehensionDict" $ annot

transpileCompFor :: CompFor Annot -> [String]
transpileCompFor (CompFor isAsync exprs inExpr iter _) =
  concat
    [ concatMap transpileExpr exprs
    , " <- "
    , transpileExpr inExpr
    ]
    : transpileCompIter iter

transpileCompIter :: Maybe (CompIter Annot) -> [String]
transpileCompIter Nothing = []
transpileCompIter (Just (IterFor compFor _)) =
  transpileCompFor compFor
transpileCompIter (Just (IterIf compIf _)) =
  transpileCompIf compIf

transpileCompIf :: CompIf Annot -> [String]
transpileCompIf (CompIf expr iter _) =
  transpileExpr expr : transpileCompIter iter

transpileDecorator :: Decorator Annot -> String
transpileDecorator (Decorator dottedName args annot) = unsupported "Decorator" annot

transpileArgument :: Argument Annot -> String
transpileArgument (ArgExpr expr _) = transpileExpr expr
transpileArgument (ArgVarArgsPos expr _) = transpileExpr expr
transpileArgument (ArgVarArgsKeyword expr _) = transpileExpr expr
transpileArgument (ArgKeyword (Ident name _) expr _) = transpileExpr expr

transpileAssignOp :: AssignOp Annot -> String
transpileAssignOp (PlusAssign annot)       = unsupported "+=" annot
transpileAssignOp (MinusAssign annot)      = unsupported "-=" annot
transpileAssignOp (MultAssign annot)       = unsupported "*=" annot
transpileAssignOp (DivAssign annot)        = unsupported "/=" annot
transpileAssignOp (ModAssign annot)        = unsupported "%=" annot
transpileAssignOp (PowAssign annot)        = unsupported "**=" annot
transpileAssignOp (BinAndAssign annot)     = unsupported "&=" annot
transpileAssignOp (BinOrAssign annot)      = unsupported "|=" annot
transpileAssignOp (BinXorAssign annot)     = unsupported "^=" annot
transpileAssignOp (LeftShiftAssign annot)  = unsupported "<<=" annot
transpileAssignOp (RightShiftAssign annot) = unsupported ">>=" annot
transpileAssignOp (FloorDivAssign annot)   = unsupported "//=" annot
transpileAssignOp (MatrixMultAssign annot) = unsupported "@=" annot

transpileOp :: Op Annot -> String
transpileOp (And _)               = "&&"
transpileOp (Or _)                = "||"
transpileOp (Not _)               = "not "
transpileOp (Exponent _)          = "^"
transpileOp (LessThan _)          = "<"
transpileOp (GreaterThan _)       = ">"
transpileOp (Equality _)          = "=="
transpileOp (GreaterThanEquals _) = ">="
transpileOp (LessThanEquals _)    = "<="
transpileOp (NotEquals _)         = "/="
transpileOp (NotEqualsV2 _)       = "/="
transpileOp (In _)                = "`elem`"
transpileOp (Is _)                = "=="
transpileOp (IsNot _)             = "/="
transpileOp (NotIn _)             = "`notElem`"
transpileOp (BinaryOr _)          = ".|."
transpileOp (Xor _)               = "`xor`"
transpileOp (BinaryAnd _)         = ".&."
transpileOp (ShiftLeft _)         = "`shiftL`"
transpileOp (ShiftRight _)        = "`shiftR`"
transpileOp (Multiply _)          = "*"
transpileOp (Plus _)              = "+"
transpileOp (Minus _)             = "-"
transpileOp (Divide _)            = "/"
transpileOp (FloorDivide _)       = "`div`"
transpileOp (MatrixMult annot)    = unsupported "@" annot
transpileOp (Invert _)            = "complement "
transpileOp (Modulo _)            = "`mod`"

unsupported :: String -> Annot -> a
unsupported err SpanEmpty = error $ "Error: " ++ err ++ " is not supported"
unsupported err (SpanCoLinear filename row start end) =
  unsupportedHelper err filename row start row end
unsupported err (SpanMultiLine filename startRow startCol endRow endCol) =
  unsupportedHelper err filename startRow startCol endRow endCol
unsupported err (SpanPoint filename row col) =
  unsupportedHelper err filename row col row col

unsupportedHelper err filename startRow startCol endRow endCol =
  error $
    concat
      [ "\nError transpiling "
      , filename
      , ":"
      , if startRow == endRow then show startRow else show startRow ++ "-" ++ show endRow
      , ":"
      , if startCol == endCol then show startCol else show startCol ++ "-" ++ show endCol
      , "\n" ++ err ++ " is not supported"
      ]
