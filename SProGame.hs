module SProGame where
    import System.Random
    
    data Player = Client | Server
                deriving (Show, Eq)

    -- | 'Stage' is just an 'Int'
    --
    -- prop> s :: Stage >= 0
    type Stage = Int

    data Module = Module { modName :: String
                         , importance :: Double
                         , reversingTime :: Stage
                         } 

    -- | 'Module's with the same name are considered equal
    instance Eq Module where
       a == b = modName a == modName b

    -- | A compact 'String' representation of a 'Module'
    instance Show Module where
       show m = modName m ++
        ":<" ++ show (importance m) ++
        "," ++ show (reversingTime m) ++ ">"
       
    -- | A 'Service' is a set of 'Module's
    type Service = [Module]

    -- | Any action targets a module or nothing ('Bottom')
    data Target = Target Module | Bottom
                deriving (Show, Eq)

    data Outcome = Prevented Target | NotPrevented Target | NoAttack
                 deriving (Show, Eq)


    totalValue :: Service -> Double
    totalValue s = sum [importance v | v <- s]

    -- | The 'score' is the utility a 'Player' gets from the 'Outcome'
    -- of 'Game' on a 'Service'
    score :: Player -> Service -> Outcome -> Double
    score _ [] _ = 0
    score Server s (Prevented _ ) = totalValue s
    -- | A penalty p, in fact it could be proportional to the
    -- reversingTime of the targeted module
    score Client _ (Prevented _ ) = -42

    score Server s (NotPrevented (Target m)) = (totalValue s) - (importance m)
    score Server s (NotPrevented Bottom) = totalValue s
    score Client _ (NotPrevented (Target m)) = importance m
    score Client _ (NotPrevented Bottom) = 0

    score Server s NoAttack = totalValue s
    score Client _ NoAttack = 0

    -- | A 'Move' is a pair of choices: (Server choice, Client choice)
    type Move = (Target, Target)

    -- | A 'Game' is a list of 'Move's indexed by 'Stage'
    type Game = [Move]

    play :: Game -> Outcome
    play [] = undefined -- Not interesting
    play g = let attack = dropWhile (\x -> snd x == Bottom)  g
                 Target j = snd (head attack)
                 attackWindow = take (1 + reversingTime j) attack
             in 
               if null attack
               then NoAttack
               else if length attackWindow < (1 + reversingTime j)
                    then NoAttack -- Maybe undefined?
                    else if elem (Target j) (fmap fst (tail attackWindow))
                         then Prevented (Target j)
                         else NotPrevented (Target j)

    -- | A 'Strategy' is a list of /non negative/ 'Int's used to compute the
    -- probability distribution of 'Target's. For
    -- example, if 'Service' has 3 'Module's, @[42, 42, 42, 42]@ will
    -- induce (via 'drawFromStrategy') a uniform distribution over
    -- each of the 'Module's plus 'Bottom' ('head' of the list).
    -- 
    -- prop> if (length (ss :: Service)) == n then (length (s :: Strategy)) == n + 1
    type Strategy = [Int]
                              
    -- | 'nextMove' is chosen randomly according to a 'Strategy'
    nextMove :: Player -> Service -> Strategy -> StdGen -> (Target, StdGen)
    nextMove _ ss s g = if j == 0
                       then (Bottom, g')
                       else (Target (ss !! (j - 1)), g')
                           where
                             (j, g') = drawFromStrategy g s



    -- | Returns the strategy for a 'Player' given the history of its
    -- moves
    getStrategy :: Player
                -> Service
                -> [Target] -- ^ history of this 'Player' moves
                -> Strategy -- ^ the 'head' of the list will be used
                            -- for 'Bottom'
    getStrategy _ s _ = replicate (n + 1) 1
        where n = length s
                            

    drawFromStrategy :: StdGen -> Strategy -> (Int, StdGen)
    drawFromStrategy g s = let cdf = tail (scanl (+) 0 s)
                               (j, g') = randomR (0, (last cdf) - 1) g
                               buckets = fmap (j >=) cdf
                           in
                             (length (takeWhile (== True) buckets), g')
                    
    -- | Make a 'Game' with a given number of 'Move's
    mkGame :: Int -> Service -> StdGen -> Game
    mkGame 0 _ _ = []
    mkGame n s g = let prev = mkGame (n - 1) s (fst (split g))
                       (shist, chist) = unzip prev
                       ssigma = getStrategy Server s shist
                       csigma = getStrategy Client s chist                 
                       (starget, g') = nextMove Server s ssigma (snd (split g))
                       (ctarget, _) = nextMove Client s csigma g'
                   in
                       prev ++ [(starget, ctarget)]

    -- | A simple simulated game 
    main :: IO ()
    main = do
      a <- return Module {modName = "a", importance = 3, reversingTime = 1}
      b <- return Module {modName = "b", importance = 2, reversingTime = 2}
      c <- return Module {modName = "c", importance = 1, reversingTime = 3}
      game <- return (mkGame 10 [a, b, c] (mkStdGen 42))
      putStrLn $ "Game: " ++ show game
      putStrLn $ "Outcome: " ++ show (play game)

