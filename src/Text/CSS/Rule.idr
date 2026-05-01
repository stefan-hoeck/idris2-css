module Text.CSS.Rule

import Data.String
import Text.CSS.Class
import Text.CSS.Declaration
import Text.CSS.Selector
import Text.HTML.Ref
import Text.HTML.Tag

%default total

public export
data Rule : (n : Nat) -> Type where
  Sel :
       (selectors : List Selector)
    -> (decls     : Declarations)
    -> Rule n

  Media :
       (query : String)
    -> (rules : List $ Rule 0)
    -> Rule 1

  Container :
       (query : String)
    -> (rules : List $ Rule 0)
    -> Rule 1

export %inline
sel : Selector -> Declarations -> Rule n
sel s = Sel [s]

export %inline
class : Class -> Declarations -> Rule n
class s = sel (class s)

export
classes : List Class -> Declarations -> Rule n
classes = sel . classes

export %inline
elem : {str : _} -> (0 tag : HTMLTag str) -> Declarations -> Rule n
elem v = sel $ elem v

export %inline
id : String -> Declarations -> Rule n
id = sel . id

export %inline
star : Declarations -> Rule n
star = sel Star

||| Uses an element ref as an ID selector
export %inline
ref : {0 t : HTMLTag s} -> Ref t -> Declarations -> Rule n
ref (Id i) = id i

export
Interpolation (Rule n) where
  interpolate (Sel s ds)    =
    let dss := fastConcat $ map interpolate ds
        ss  := fastConcat . intersperse ", " $ map interpolate s
     in "\{ss}{\{dss}}"
  interpolate (Media q rs)  = "@media (\{q}){\{unlines $ map interpolate rs}}"
  interpolate (Container q rs) = "@container (\{q}){\{unlines $ map interpolate rs}}"

||| Convenience alias for `List (Rule 0)`
public export
0 Rules0 : Type
Rules0 = List (Rule 0)

||| Convenience alias for `List (Rule 1)`
public export
0 Rules : Type
Rules = List (Rule 1)
