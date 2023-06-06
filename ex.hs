

----------------------------------------------------------------
--5.1
data TipoProduto = Escritorio | Informatica | Livro | Filme | Total
data Produto = Produto { valor :: Double, tp :: TipoProduto } | Nada
  deriving ()

instance Semigroup Produto where
    Nada <> p = p
    p <> Nada = p
    Produto v1 tp1 <> Produto v2 tp2 = Produto (v1 + v2) Total

instance Monoid Produto where
    mempty :: Produto
    mempty = Nada
    
----------------------------------------------------------------
--5.2 

totalGeral :: [Produto] -> Double
totalGeral produtos = valor total
  where total = foldr mappend mempty produtos

----------------------------------------------------------------
--5.4

newtype Min a = Min { getValue :: a }
  deriving (Eq, Ord, Show)

instance Ord a => Semigroup (Min a) where
  (Min x) <> (Min y) = Min (min x y)

minAll :: (Ord a, Bounded a) => [Min a] -> Min a
minAll = foldr (<>) (Min maxBound)

