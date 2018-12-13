module Main where
import Data.List (isPrefixOf, notElem)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Char (isAlphaNum)
--import Data.Text (Text)
--import qualified Data.Text as T

type Line = Vector String  -- can be only part of input line
type Var = Map String Line
type Rule = Map String Line

process :: Var -> Rule -> IO ()
process vars rules = do
    line <- getLine
    -- parse l
    let vl = V.fromList $ words line
        isCommand = isQuit || isShowRules
        isQuit = fstEl == "quit!"
        isShowRules = fstEl == "rules"
        isNo_op
            | isBlank = True
            | isComment = True
            | otherwise = False
        isComment = hdFstEl == '%'
        isBlank = V.length vl == 0
        isRule
            | simpleR = True
            | otherR = True
            | otherwise = False
        simpleR = lsFstEl == '.'
                    || rel1 && lsLstEl == '.'
        otherR = False  -- tbd 
        fstEl = vl V.! 0
        lstEl = V.last vl
        hdFstEl = head fstEl
        lsLstEl = last lstEl
        lsFstEl = last fstEl
        len = length $ fstEl
        rel1 = hdFstEl == '['
        isQuery = False
        ok = putStrLn "Ok"
        bye = putStrLn "Bye."
        rubbish = putStrLn "This is rubbish"
    case () of
        _
            | isNo_op -> process vars rules       
            | isQuit -> do
                bye
                return ()
            | isShowRules -> do
                showRules vars rules
                process vars rules
            | isQuery  -> query vl
            | isRule -> do
                let (vars_, rules_) = rule vl vars rules
                ok
                process vars_ rules_
            | True -> do 
                rubbish
                process vars rules



query vl = undefined
rule :: Line -> Var -> Rule -> (Var, Rule)
rule l vars rules
    | isL1 && isVar =       -- variable
        let newVars = M.insert name (V.fromList [""]) vars
            newRules = M.insert name (V.fromList [name]) rules
        in (newVars, newRules)
    | isL1 =                -- name
        let newRules = M.insert name (V.fromList [""]) rules
        in (vars, newRules)
    | isRel =               -- relational term
        let newRules = M.insert nmFlt elmts rules
            nmFlt = filBeg name
            elmts = V.map init elmts_
            elmts_ = filEnd $ V.tail l
            filEnd = V.filter (all (/= ']')) 
            filBeg = filter (/= '[')
        in (vars, newRules)
    | otherwise = (M.singleton "shit" (V.fromList ["what","the","fuck"]), M.empty) where
    name = init $ l V.! 0
    isL1 = V.length l == 1
    isVar = (head $ l V.! 0) == '#'
    isRel = V.length l > 0
    

showRules vars rules = do
    putStrLn $ "rules: " ++ show (M.assocs rules)
    putStrLn $ "variables: " ++ show (M.assocs vars)

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
