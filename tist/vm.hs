
type State = ([Int], [Int], Int, Int)

exec :: State -> IO ()
exec s0@(stack,instrs,pc,fp) =
    let instr = instrs !! pc
    in
      case instr of
        0 -> return ()
        14 -> do putStr $ (show $ head stack) ++ "\n"
                 exec (tail stack, instrs, pc + 1, fp - 1)
        otherwise -> exec $ step instr s0

step :: Int -> State -> State
step instr (stack,instrs,pc,fp) =
    case instr of
      1 -> ((replicate next 0) ++ stack, instrs, pc2, fp + next)
      2 -> (next:stack, instrs, pc2, fp+1)
      3 -> ((stack!!(fp - next)):stack, instrs, pc2, fp + 1)
      4 -> (tail $ set_nth stack (fp - next) first, instrs, pc2, fp1)
      5 -> ((second+first):drop2, instrs, pc1, fp1)
      6 -> ((second-first):drop2, instrs, pc1, fp1)
      7 -> ((second*first):drop2, instrs, pc1, fp1)
      8 -> ((div second first):drop2, instrs, pc1, fp1)
      9 -> (stack, instrs, next, fp)
      10 -> if second /= first then (drop2, instrs, next, fp2)
            else (drop2, instrs, pc2, fp2)
      11 -> if second >= first then (drop2, instrs, next, fp2)
            else (drop2, instrs, pc2, fp2)
      12 -> if second <= first  then (drop2, instrs, next, fp2)
            else (drop2, instrs, pc2, fp2)
      13 -> ([first,fp,pc2] ++ tail stack, instrs, next, 0)
      15 -> let fp' = stack !! (fp + 1)
                pc' = stack !! (fp + 2)
            in
              (first : drop (fp+3) stack, instrs, pc', fp')
    where next = instrs !! (pc+1)
          first = head stack
          second = stack !! 1
          drop2 = drop 2 stack
          fp1 = fp - 1
          fp2 = fp - 2
          pc1 = pc + 1
          pc2 = pc + 2

set_nth :: [a] -> Int -> a -> [a]
set_nth (x:xs) n a
    | n == 0 = a:xs
    | otherwise = x:(set_nth xs (n - 1) a)


main :: IO ()
main =
    do prog <- getLine
       let ints = read prog::[Int]
           s0 = ([],ints,0,0)
       exec s0

