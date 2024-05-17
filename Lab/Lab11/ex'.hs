import Prelude hiding (Left, Right)

data Tree = Node Int [Tree] deriving (Show, Eq)

data Crumb'' = Crumb'' Int [Tree] [Tree] deriving (Show, Eq)
type Trail'' = [Crumb'']
type Zipper'' = (Tree, Trail'')