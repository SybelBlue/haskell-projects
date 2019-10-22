import Data.List.Split
import Data.String (unwords)
import Data.Char (isUpper)

solution :: String -> String
solution = unwords . split . dropBlanks . keepDelimsR . (whenElt isUpper)
