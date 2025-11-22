module Text.CSS.Class

import Derive.Prelude

%default total
%language ElabReflection

||| A CSS class
public export
record Class where
  constructor C
  value : String

%runElab derive "Class" [Show,Eq,Ord,FromString]

export %inline
Interpolation Class where interpolate = value

public export
0 Classes : Type
Classes = List Class
