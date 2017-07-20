
data Jug = A | B | C deriving Show

data Move = FromTo Jug Jug deriving Show
type Path = [Move]

type State = ((Int,Int,Int), Path)

start :: State
start = ((8,0,0), [])

successors :: State -> [State]

successors ((a,b,c),path) =
    let ab = min a (5 - b)
        ac = min a (3 - c)
        ba = min b (8 - a)
        bc = min b (3 - c)
        ca = min c (8 - a)
        cb = min c (5 - b)
        ss = [(ab, a-ab, b+ab,    c, path ++ [FromTo A B]),
              (ac, a-ac,    b, c+ac, path ++ [FromTo A C]),
              (ba, a+ba, b-ba,    c, path ++ [FromTo B A]),
              (bc,    a, b-bc, c+bc, path ++ [FromTo B C]),
              (ca, a+ca,    b, c-ca, path ++ [FromTo C A]),
              (cb,    a, b+cb, c-cb, path ++ [FromTo C B])]
    in
      [((a',b',c'), path') | (amount,a',b',c',path') <- ss, amount > 0]

search :: [State] -> Path

search (s:ss)
    | fst s == (4,4,0) = snd s
    | otherwise = search $ ss ++ successors s
