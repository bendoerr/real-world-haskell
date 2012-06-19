data MyTree a = Node a (Maybe (MyTree a)) (Maybe (MyTree a))
                deriving (Show)
