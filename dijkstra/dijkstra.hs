import           Data.Function      (on)
import           Data.List.NonEmpty (NonEmpty ((:|)), toList, (<|))
import qualified Data.Map           as M
import           Data.Monoid        (Sum (Sum))
import qualified Data.PQueue.Min    as P

data Node e v
  = Node
  { nodeValue :: v
  , nodeEdges :: [Edge e v]
  } deriving Show

-- A workaround to avoid inifinite comparison of cyclic nodes:
-- `nodeValue`s should be distinct.
instance Ord v => Ord (Node e v) where
  compare = compare `on` nodeValue
instance Eq v => Eq (Node e v) where
  (==) = (==) `on` nodeValue

data Edge e v
  = Edge
  { edgeValue  :: e
  , edgeTarget :: Node e v
  } deriving Show

data Path e v
  = Path
  { pathValue :: e
  , pathNodes :: NonEmpty (Node e v)
  } deriving Show

newPath n = Path mempty (n :| [])
(Edge e n) `addTo` (Path v ns) = Path (e <> v) (n <| ns)
pathHead (Path _ (n :| _)) = n

instance Ord e => Ord (Path e v) where
  compare = compare `on` pathValue
instance Eq e => Eq (Path e v) where
  (==) = (==) `on` pathValue

dijkstra :: (Monoid e, Ord e, Ord v) => Node e v -> M.Map (Node e v) (Path e v)
dijkstra start = loop q M.empty
  where q = P.singleton . newPath $ start
        loop q r
          = case P.splitAt 1 q of
              ([p], q')
                | M.member (pathHead p) r -> loop q' r
                | otherwise
                  -> loop (updateNeighborPaths q' p)
                          (M.insert (pathHead p) p r)
              ([] , _ ) -> r
              _         -> error "splitAt 1"
        updateNeighborPaths q p
          = foldr P.insert q paths
          where paths = map (`addTo` p) . nodeEdges . pathHead $ p

valueMap :: Ord v => M.Map (Node e v) (Path e v) -> M.Map v (e, [v])
valueMap
  = M.mapKeys nodeValue
  . M.map (\p -> (pathValue p, map nodeValue . toList . pathNodes $ p))

g1 = a
  where a = Node 'a' [Edge (Sum 4) b, Edge (Sum 2) d]
        b = Node 'b' [Edge (Sum 1) c]
        c = Node 'c' []
        d = Node 'd' [Edge (Sum 1) b, Edge (Sum 3) c]

g2 = a
  where a = Node 'a' [Edge (Sum 1) b]
        b = Node 'b' [Edge (Sum 1) c]
--        c = Node 'c' []
        c = Node 'c' [Edge (Sum 1) a]
