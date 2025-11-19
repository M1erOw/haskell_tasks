import Prelude hiding (Nothing)

data MaybeNode a
    = Nothing
    |Node a (MaybeNode a) (MaybeNode a)



mapTree :: (a -> a) -> MaybeNode a -> MaybeNode a
mapTree _ Nothing = Nothing
mapTree f (Node x y z) = Node (f x) (mapTree f y) (mapTree f z)

countNodes :: MaybeNode a -> Int
countNodes Nothing = 0
countNodes (Node x y z) = 1 + countNodes y + countNodes z

tree = Node 9 
        (Node 0 
            (Node 4 Nothing Nothing)
            (Node 1
                (Node 7
                    Nothing
                    Nothing
                )
                (Node 5
                    Nothing
                    Nothing
                )
            )
        )
        (Node 2
            (Node 0 
                Nothing 
                Nothing
            )
            (Node 1 
                Nothing 
                Nothing
            )
        )


func1 a b = a:b
func2 a b = b ++ [a]

treeTraverseD :: (a -> b -> b) -> b -> MaybeNode a -> b
treeTraverseD _ b Nothing = b
-- treeTraverseD f b (Node x Nothing Nothing) = f x b 
-- treeTraverseD f b (Node x y Nothing) = f x (treeTraverseD f b y)
-- treeTraverseD f b (Node x Nothing z) = treeTraverseD f (f x b) z
treeTraverseD f b (Node x y z) = treeTraverseD f (f x (treeTraverseD f b y)) z

{- 
ghci> treeTraverseD func2 [] tree
[4,0,7,1,5,9,0,2,1]
 -}

treeTraverseW :: (a -> b -> b) -> b -> MaybeNode a -> b
treeTraverseW _ b Nothing = b
treeTraverseW f b (Node x y z) = treeTraverseW f (treeTraverseW f (f x b) y) z

{- 
ghci> treeTraverseW func2 [] tree
[9,0,4,1,7,5,2,0,1]
 -}
 
------------ РЕАЛИЗАЦИЯ --------------
-- nicePrint :: Show a => MaybeNode a -> IO ()
-- nicePrint tree = h "|---" tree where
--     h _ Nothing = putStr ""
--     h oldBase (Node x Nothing Nothing) =
--         print x
--     h oldBase (Node x Nothing z) = do
--         print x
--         let count = length oldBase - 4
--         putStr oldBase
--         h (take count oldBase ++ "    " ++ drop count oldBase) z
--     h oldBase (Node x y Nothing) = do
--         print x
--         putStr oldBase     
--         let count = length oldBase - 4
--         h (take count oldBase ++ "    " ++ drop count oldBase) y
--     h oldBase (Node x y z) = do
--         print x
--         putStr oldBase
--         let count = length oldBase - 4
--         h (take count oldBase ++ "|   " ++ drop count oldBase) y 
--         putStr oldBase
--         h (take count oldBase ++ "    " ++ drop count oldBase) z


-------------- РЕАЛИЗАЦИЯ Покрасивее -----------------
nicePrint :: Show a => MaybeNode a -> IO ()
nicePrint = h "|---" where
    h _ Nothing = putStr ""
    h oldBase (Node x y z) = do
        print x
        let count = length oldBase - 4
            base = take count oldBase
            rest = drop count oldBase
            proccessChild child prefix = do
                putStr oldBase
                h (base ++ prefix ++ rest) child
            g Nothing Nothing = putStr ""
            g left Nothing = proccessChild left "    "
            g Nothing right = proccessChild right "    "
            g left right = do
                proccessChild left "|   "                 
                proccessChild right "    "
        g y z

{- 
ghci> nicePrint tree
9        
|---0    
|   |---4
|   |---1
|       |---7
|       |---5
|---2
    |---0
    |---1  
-}

