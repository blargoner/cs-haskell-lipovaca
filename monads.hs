import Prelude hiding (lookup)
import Data.Map
import Control.Monad

-- example
-- since Maybe is a monad, we can perform sequenced left folds with operators
-- that might fail, like Map lookups
lookup' :: (Ord a) => a -> [Map a a] -> Maybe a
lookup' = foldM lookup

cityCounty = fromList [("Berkeley", "Alameda"), ("Chicago", "Cook")]
countyState = fromList [("Alameda", "California"), ("Cook", "Illinois")]
stateCountry = fromList [("California", "USA"), ("Illinois", "USA")]

berkeleyCountry = lookup' "Berkeley" [cityCounty, countyState, stateCountry]