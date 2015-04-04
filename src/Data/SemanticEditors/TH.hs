module Data.SemanticEditors.TH
  ( mkEditors
  , mkEditor
  , mkConstrTests
  )
where

import Control.Applicative
import Data.Char (toUpper)
import Data.Maybe (isJust, fromJust)
import Language.Haskell.TH.Syntax


infix 1 <.> -- chosen arbitrarily
f <.> g = (f <$>) . g

-- |mkEditors creates Semantic Editor Combinators for each data type given. 
--  More information see mkEditor
mkEditors :: [Name] -> Q [Dec]
mkEditors = concat <.> mapM mkEditor

-- |mkEditor creates Semantic Editor Combinators for each named field in a given data type by
--  appending the fields name (first letter is converted to uppercase) to the name \"edit\".
--  If a fields name starts with an underscore \'_\' no editor will be generated
--
--  for example:
--
-- >  data Person = Person { age :: Integer, name :: String, _sex :: String }
--
--  will generate the lifters  editAge and editName:
--
-- @
--    editAge  f p = p { age = f (age p) }
--    editName f p = p { name = f (name p) }
-- @
--
mkEditor :: Name -> Q [Dec]
mkEditor name = do
    i <- reify name
    map (fromJust) . filter (isJust) <.> mapM mkEditor' . concatMap vars $
      case i of
        TyConI (DataD _ _ _ cs _) -> cs
        TyConI (NewtypeD _ _ _ c _) -> [c]
        _ -> []

  where vars (RecC _ v) = v

mkEditor' (name, _, _) = case nameBase name of
                           ('_':_)  -> return Nothing
                           (c:rest) -> Just <$> mkEditor'' ("edit" ++ (toUpper c:rest))
  where 
    mkEditor'' :: String -> Q Dec
    mkEditor'' name' = return $
      FunD (mkName name')
           [Clause [VarP (mkName "f"), VarP (mkName "r")] (NormalB $ 
                   RecUpdE (VarE (mkName "r")) 
                           [(name, 
                             AppE (VarE (mkName "f")) 
                                  (AppE (VarE name) (VarE $ mkName "r")))
                           ]) []]

-- |Template Haskell function for automatically creating predicates testing the constructors of a 
--  given data type.
--  for example:
--
-- @
--   data Color = Red | Green | Blue
--  $(mkConstrTests [''Color])
-- @
  --
--  will generate the following functions:
--
-- @
--   isRed Red     = True
--   isRed _       = False
--   isGreen Green = True
--   isGreen _     = False
--   isBlue Blue   = True
--   isBlue _      = False
-- @
--
mkConstrTests :: [Name] -> Q [Dec]
mkConstrTests = concat <.> mapM mk 
  where
    mk name = do
      i <- reify name
      map fromJust . filter isJust <.> mapM mkPredicate $
        case i of
          TyConI (DataD _ _ _ cs _) -> cs
          _ -> []

    mkPredicate (NormalC name ts) = Just <$> mkPredicate' name (length ts)
    mkPredicate (RecC name ts)   = Just <$> mkPredicate' name (length ts)
    mkPredicate _                = return Nothing

    mkPredicate' name argc = return $
      FunD (predName name)
        [ Clause [ConP name $ replicate argc WildP] (NormalB $ ConE (mkName "True")) []
        , Clause [WildP] (NormalB $ ConE (mkName "False")) []
        ]

    predName name = mkName ("is" ++ nameBase name)

