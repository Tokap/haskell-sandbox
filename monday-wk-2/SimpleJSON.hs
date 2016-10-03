data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject [(JString, JValue)]
            | JArray [JValue]
              deriving (Eq, Ord, Show)
