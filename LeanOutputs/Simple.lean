class YesNo (a : Type) where
    yesno : a -> Bool

open YesNo


instance : YesNo Int where
yesno a := 
match a with
| 0 => false

|  _  => true




instance : YesNo (List a) where
yesno a := 
match a with
| []  => false

|  _  => true




instance : YesNo Bool where
yesno  := id




instance : YesNo (Option a) where
yesno a := 
match a with
| (some  _ ) => true

| none  => false




def main  := do
    IO.print $ yesno $ some 0
    IO.print $ yesno true
    IO.print $ yesno [1, 2, 3]


