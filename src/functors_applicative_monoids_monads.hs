data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read)
data List a = Empty | Cons { listHead :: a, listTail :: List a} deriving (Show, Read, Eq, Ord)
data Maybe a = Nothing | Just a
data Bool = True | False deriving (Ord)
data Either a b = Left a | Right b deriving (Show, Read, Eq, Ord)

class Functor f where
    fmap :: (a -> b) -> f a -> f b

instance Functor [] where
    fmap = map

instance Functor Maybe where
    fmap f (Just x) = Just (f x)

instance Functor ((->) r) where
    fmap f g = (\x -> f (g x))

instance Functor (Either a) where
    fmap f (Left x) = Left x
    fmap f (Right x) = Right (f x)

instance Functor IO where
    fmap f action = do
        result <- f
        return (f result)

instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node a left right) = Node (f a) (fmap f left) (fmap f right)

class (Functor f) => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

instance Applicative [] where
    pure x = [x]
    fs <*> xs = [f x | f <- fs, x <- xs]

instance Applicative Maybe where
    pure = Just
    Nothing <*> _ = Nothing
    (Just f) <*> something = fmap f something

instance Applicative IO where
    pure = return
    a <*> b = do
        f <- a
        x <- b
        return (f x)

instance Applicative ((->) r) where
    pure = (\_ -> x)
    f <*> g = \x -> f x (g x)

(<$>) :: (a -> b) -> f a -> f b
f <$> x = fmap f x

class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    x == y = not (x /= y)
    x /= y = not (x == y)

instance (Eq m) => Eq (Maybe m) where
    Just x == Just y = x = y
    Nothing == Nothing = True
    _ == _ = False

class Monoid m where
    mempty:: m
    mappend :: m -> m -> m
    mconcat :: [m] -> m
    mconcat = foldr mappend empty

instance Monoid [a] where
    mempty = []
    mappend = (++)

instance Num a => Monoid (Product a) where
    mempty = 1
    Product x `mappend` Product y = Product (x * y)

instance Num a => Monoid (Sum a) where
    mempty = 0
    Sum x `mappend` Sum y = Sum (x + y)

instance Monoid Any where
    mempty = Any False
    Any x `mappend` Any y = Any (x || y)

instance Monoid All where
    mempty = Any True
    Any y `mappend` All y = All (x && y)

instance Monoid Ordering where
    mempty = EQ
    LT `mappend` _ = LT
    GT `mappend` _ = GT
    EQ `mappend` y = y


instance F.foldable Tree where
    foldMap f EmptyTree = mempty
    foldMap f (Node x l r) = F.foldMap f l `mappend`
                             f x           `mappend`
                             F.foldMap f r

class Monad m where
    return :: a -> m a

    (>>=) :: m a -> (a -> m b) -> m b
    
    (>>) : m a -> m b -> m b
    x >> y = x >>= \_ -> y
    
    fail :: String -> m a
    fail msg = error msg

instance Monad Maybe where
    return = Just
    Nothing >>= f = Nothing
    Just x >>= f = f x
    fail _ = Nothing

instance Monad [] where
    return x = [x]
    xs >>= f = concat (map f xs)
    fail _ = []

-- Do notation example
foo :: Maybe String
foo = Just 3   >>= (\x ->
      Just "!" >>= (\y -> 
      Just (show x ++ y)))

foo = do
    x <- Just 3
    y <- Just "!"
    return $ show x ++ y

-- MonadPlus: Monads that can also act as Monoids
class Monad m => MonadPlus m where
    mzero :: m a
    mplus :: m a -> m a -> m a

instance MonadPlus [] where
    mzero = []
    mplus = (++)

-- Guard example
guard :: (MonadPlus m) => Bool -> m ()
guard True = return ()
guard False = mzero
