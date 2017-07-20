
data JugID = A | B | C deriving (Eq,Show)

data Jug = Jug JugID Int deriving (Eq,Show)

data Move = FromTo JugID JugID deriving Show
type Path = [Move]

type State = ([Jug], Path)

start :: State
start = ([Jug A 8, Jug B 0, Jug C 0], [])

capacity :: JugID -> Int

capacity A = 8
capacity B = 5
capacity C = 3

successors :: State -> [State]

successors (jugs,path) =
      [(let move = min af (capacity b - bf) in
         [Jug a (af-move), Jug b (bf+move), Jug c cf], path ++ [FromTo a b])
          | Jug a af <- jugs, Jug b bf <- jugs, Jug c cf <- jugs,
            af > 0, capacity b > bf, a /= b, b /= c, c /= a]

search :: [State] -> Path

search (s:ss)
    | Jug A 4 `elem` fst s && Jug B 4 `elem` fst s = snd s
    | otherwise = search $ ss ++ successors s
