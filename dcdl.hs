module DCDL where

data Circuit = Circuit Gates Links
data Gates = Gate Int GateFn Gates | NoGates
data Links = Link Int Int Int Int Links | NoLinks
data GateFn = And | Or | Not | Xor
              deriving Show

halfadder = Circuit (Gate 1 Xor (Gate 2 And (NoGates)))
            (Link 1 1 2 1 (Link 1 2 2 2 (NoLinks)))

ppc :: Circuit -> String
ppc (Circuit gates links) = ppg gates ++ ppl links
instance Show Circuit where show = ppc

ppg :: Gates -> String
ppg (Gate i fn gates) = show i ++ ":" ++ show fn ++ ";\n" ++ ppg gates
ppg (NoGates) = ""

ppl :: Links -> String
ppl (Link g0 i0 g1 i1 links) = "from " ++ show g0 ++ ":" ++ show i0 ++ " to " ++
                               show g1 ++ ":" ++ show i1 ++ ";\n" ++ ppl links
ppl (NoLinks) = ""
