

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

-----------------------------------------------------------------
--7.5
data Fantasma a = Fantasma

instance Functor Fantasma where
  fmap _ Fantasma = Fantasma

----------------------------------------------------------------
  --7.2 honestamente entendi nada

  data Dupla a = Dupla a Int a

instance Functor Dupla where
  fmap f (Dupla x n y) = Dupla (f x) n (f y)

  ----------------------------------------------------------------
  --8.1 

  instance Functor Caixa where
  fmap f (Um a) = Um (f a)
  fmap f (Dois a b) = Dois (f a) (f b)
  fmap f (Tres a b c) = Tres (f a) (f b) (f c)


  ----------------------------------------------------------------
  --8.2

  import Control.Monad

data Caixa a = Caixa  a a a deriving Show

mult234 :: Double -> Caixa Double
mult234 x = (\a -> return (2 * a)) x >>= (\b -> return (3 * b)) >>= (\c -> return (4 * c))

instance Functor Caixa where
  fmap f (Caixa x y z) = Caixa (f x) (f y) (f z)

instance Applicative Caixa where
  pure a = Caixa a a a
  (Caixa f g h) <*> (Caixa x y z) = Caixa (f x) (g y) (h z)

instance Monad Caixa where
  (Caixa x y z) >>= f = Caixa x' y' z'
    where
      Caixa x' _ _ = f x
      Caixa _ y' _ = f y
      Caixa _ _ z' = f z


----------------------------------------------------------------
--8.3
-- Tres 1 2 3 >>= mult234 >>= mult234:

-- Começamos com a expressão Tres 1 2 3, que cria uma caixa com os valores 1, 2 e 3 nas coordenadas.
-- Em seguida, utilizamos o operador >>= para aplicar a função mult234 a esses valores.
-- A função mult234 multiplica cada valor por 2, 3 e 4 nas coordenadas correspondentes.
-- Portanto, o resultado será a caixa com os valores resultantes: Tres 2 6 12.
-- Dois 2 4 >>= mult234:

-- Começamos com a expressão Dois 2 4, que cria uma caixa com os valores 2 e 4 nas coordenadas.
-- Em seguida, utilizamos o operador >>= para aplicar a função mult234 a esses valores.
-- A função mult234 multiplica cada valor por 2, 3 e 4 nas coordenadas correspondentes.
-- Portanto, o resultado será a caixa com os valores resultantes: Dois 4 12.
-- :kind Coisa:

-- O comando :kind é usado no GHCi para obter o tipo de uma expressão.
-- No caso de Coisa, não é possível determinar o tipo sem a definição do tipo Coisa. Portanto, não é possível determinar o valor dessa expressão sem mais informações.
-- Dois 2 3 >>= \_ -> Dois 7 7:

-- Começamos com a expressão Dois 2 3, que cria uma caixa com os valores 2 e 3 nas coordenadas.
-- Em seguida, utilizamos o operador >>= para aplicar a função lambda \_ -> Dois 7 7 a esses valores.
-- A função lambda ignora o valor anterior e retorna a caixa Dois 7 7.
-- Portanto, o resultado será a caixa Dois 7 7.