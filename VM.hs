data VM = VM { counter :: Int, registers :: [Register] } deriving (Show, Eq)
data Register a = Register { value :: a }



