------------------------- SAMPLE QUESTION 1 ------------------------------------
-- Write function toInitials returs initials for a given person name.
-- E.g: "Bill Gates" -> "B. G."

module  Initials where

toInitials :: String -> String

toInitials [] = []
toInitials xs = [firstInitial] ++ ". " ++ [secondInitial] ++ ['.']
                where brokenName = words xs
                      firstInitial = head (brokenName !! 0)
                      secondInitial = head (brokenName !! 1)
