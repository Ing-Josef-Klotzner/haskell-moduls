module Main where
import Data.List (isPrefixOf, notElem)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Maybe (fromJust)
--import Data.Text (Text)
--import qualified Data.Text as T

type Line = Vector String  -- can be only part of input line
type Var = Map String Line
type Rule = Map String Line

process :: Var -> Rule -> IO ()
process vars rules = do
    line <- getLine
    -- parse l
    let vl = V.fromList $ words line :: Line
        isCommand = isQuit || isShowRules
        isQuit = fstEl == "quit!"
        isShowRules = fstEl == "rules"
        isNo_op = isBlank || isComment
        isComment = hdFstEl == '%'
        isBlank = V.length vl == 0
        isRule = simpleR || otherR
        simpleR = lsFstEl == '.' || rel1 && lsLstEl == '.'
        otherR = hdFstEl == '{' && lsLstEl == '.' 
        fstEl = vl V.! 0
        lstEl = V.last vl
        hdFstEl = head fstEl
        lsFstEl = last fstEl
        lsLstEl = last lstEl
        len = length $ fstEl
        vlen = V.length vl
        rel1 = hdFstEl == '['
        isL1 = vlen == 1
        isVar = (head $ fstEl) == '#'
        isRel = rel1
        name = init $ fstEl
        isQuery = False
        ok = putStrLn "Ok"
        bye = putStrLn "Bye."
        rubbish = putStrLn "This is rubbish"
        query vl = undefined
        rule :: (Var, Rule)
        rule
            | isL1 && isVar =       -- variable
                let newVars = M.insert name (V.singleton ("")) vars
                    newRules = M.insert name (V.singleton name) rules
                in (newVars, newRules)
            | isL1 =                -- name
                let newRules = M.insert name (V.fromList [""]) rules
                in (vars, newRules)
            | isRel =               -- relational term
                let newRules = M.insert nmFlt elmts rules
                    nmFlt = filBeg name
                    elmts = filEnd $ V.tail vl 
                    filEnd = V.map (filter (\c -> c /= ',' && c /= ']' && c /= '.')) 
                    filBeg = filter (/= '[')
                in (vars, newRules)
            -- "{(" <complex-terms> ") => " <simple-term> "}."
            | otherR =
                let newRules = M.insert nameOth elmts rules 
                    nameOth = filName $ unwords $ V.toList $ el1
                    el1 = V.slice 0 (grtEq -1) vl
                    elmts = fil $ V.slice grtEq (vlen - grtEq) vl
                    filName = filter (\c -> c /= ',' && c /= '{' && c /= '.'
                            && c /= '(' && c /= ')' && c /= ':')
                    fil = V.map $ filter (\c -> c /= '.' && c /= '}' && c /= ':' && c /= '[' && c /= ']')
                    grtEq = (fromJust $ V.findIndex (== "=>") vl) + 1
                    -- check elmts for variables
                    namOfEl1 = filter (/= '[') $ el1 V.! 0
                    el1C = filter (/= ']') $ el1 V.! 1
                    isVa
                        | length el1C > 1 = if head el1C == '#' then True else False
                        | otherwise = False 
                    mBVarRule = M.lookup namOfEl1 newRules
                    strRl = case mBVarRule of
                                    Nothing -> V.singleton "Nothing"
                                    Just c -> c
                    newVars = if strRl /= (V.singleton el1C) && isVa && mBVarRule /= Nothing
                                then M.insert el1C strRl vars else vars
                in (newVars, newRules)
            | otherwise = (M.singleton "shit" (V.fromList ["what","the","fuck"]), M.empty) where
        showRules = do
            putStrLn $ "rules: " ++ show (M.assocs rules)
            putStrLn $ "variables: " ++ show (M.assocs vars)
    case () of
        _
            | isNo_op -> process vars rules       
            | isQuit -> do
                bye
                return ()
            | isShowRules -> do
                showRules
                process vars rules
            | isQuery  -> query vl
            | isRule -> do
                let (vars_, rules_) = rule
                ok
                process vars_ rules_
            | True -> do 
                rubbish
                process vars rules

main :: IO ()
main = do
    let vars = M.empty
    let rules = M.empty
    process vars rules

{-  grammar:
<variable> ::= "#" <name>
<relational-term> ::= "[" <name> ": " <simple-term> <simple-terms> "]"
<simple-term> ::= <name> | <variable> | <relational-term>
<simple-terms> ::= "" | ", " <simple-term> <simple-terms>
<equality-assertion> ::= "<" <simple-term> " = " <simple-term> ">"
<non-equality-assertion> ::= "<" <simple-term> " /= " <simple-term> ">"
<complex-term> ::= <simple-term> | <equality-assertion> | <non-equality-assertion>
<complex-terms> ::= "" | <complex-term> <complex-terms-1>
<complex-terms-1> ::= "" | ", " <complex-term> <complex-terms-1>
<rule> ::= <simple-term> "." | "{(" <complex-terms> ") => " <simple-term> "}."
<query> ::= "(" <complex-terms> ")?"
<command> ::= "quit!"
<no_op> ::= "" | "%" <comment>
<op> ::= <rule> | <query> | <command> | <no_op>
<input-line> ::= <op> <EOL>
-}
