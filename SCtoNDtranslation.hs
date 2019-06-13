module Trl where

-- | Formuła
data Form = V Int       -- variable
          | F           -- falsum
          | C Form Form -- conjunction
          | D Form Form -- dysjunction
          | I Form Form -- implication
          | N           -- null, empty context
          deriving(Eq, Read)

instance Show Form where
    show (V i)   = "V" ++ show i
    show F       = "F"
    show (C a b) = show a ++ " ^ " ++ show b
    show (D a b) = show a ++ " v " ++ show b
    show (I a b) = show a ++ " -> " ++ show b
    show N       = "null"

-- the type for the premisses of SC rules
type SCpremiss = [([Form], Form)]
-- the type for the conclusion of SC rules
type SCconcl = ([Form], Form)
-- the type for the premisses of ND rules
type NDpremiss = ([Form], [Form])
-- the type for the comclusion of ND rules
type NDconcl = Form

-- the type of a SC rule
type RuleSC = (SCpremiss, SCconcl)
-- the type of a ND rule
type RuleND = (NDpremiss, NDconcl)

-- filters 'null' aka contexts
filterN :: [Form] -> [Form]
filterN x = filter (/=N) x
-- auxilary functions to match types
mapSnd x = map snd x
mapFst x = concat(map fst x)


-- translates right SC rules to ND introduction rules
intr :: RuleSC -> RuleND
intr (x, y) = ((mapSnd x, filterN (mapFst x)), snd y)

--Right conjunction rule
p1 = ([([N], V 1), ([N], V 2)], ([N], C (V 1) (V 2 )))
--Right disjunction rule 1
p2 = ([([N], V 1)], ([N], D (V 1) (V 2)))
--Right disjunction rule 2
p2b = ([([N], V 2)], ([N], D (V 1) (V 2)))
--R implication rule
p3 = ([([N, V 1], V 2)], ([N], I (V 1) (V 2)))

elim :: RuleSC -> RuleND
elim (x,y) = (([head (fst y)] ++ (mapSnd x), filterN (mapFst x)), snd y)

-- Left conjunction rule
p4 = ([([V 1, V 2, N], V 3)], ([C (V 1) (V 2), N], V 3))
-- Left disjunction rule
p5 = ([([V 1, N], V 3), ([V 2, N], V 3)], ([D (V 1) (V 2), N], V 3))
-- Left implication rule
p6 = ([([N], V 1), ([V 2, N], V 3)], ([I (V 1) (V 2), N], V 3))