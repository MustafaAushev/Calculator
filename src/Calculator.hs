module Calculator where


type ExprParser = String -> (Float, String, Bool)

unary_functions = [("ln",log), ("exp", exp), ("sin", sin), ("cos", cos), ("inc", (+) 1)]
binary_functions= [("pow", (**)), ("add", (+)), ("sub",(-)), ("mul", (*)), ("div",(/))]
constants      = [("pi", pi), ("e", exp 1)]


calculator :: String -> Float


find_table_element :: String -> [(String, a)] -> a -> (a, String, Bool)

failParse s = (0, s, False)
isSucceed (val, s, res) = res
getVal (val, s, res) = val
getEndStr (val, s, res) = s

check_str :: String -> String -> (Bool, String)
check_str [] s2 = (True, s2)
check_str s1 [] = (False, [])
check_str s1 s2 = if (head(s1) == head(s2) ) then check_str (tail s1) (tail s2)
                        else (False, s2)
                      
infixr 4 >|
(>|) :: ExprParser -> ExprParser -> ExprParser
(p1 >| p2) s = if isSucceed (p1 s) then p1 s else p2 s

------------------------------------------------------------
calculator s = getVal (expr (filter (\x-> x /= ' ') s))

------------------------------------------------------------
expr s = (addsub_expr >| term2) s
                
------------------------------------------------------------
number s = case readsPrec 10 s of
                x:xs -> (fst x, snd x, True)
                [] -> failParse s

------------------------------------------------------------
parentheses s@(x:xs) = if (x /= '(') then failParse s
                       else case expr xs of
                        (_, fs, False) -> failParse fs
                        (val, xr:sr, True) -> if (xr == ')') then (val, sr, True) else error "parse error: incorent parentheses"
                        (_, [], _) -> error "parse error: expression parsing failed"                                                
                                
------------------------------------------------------------
minus_term s@(x:xs) = if (x == '-') then
                        if isSucceed (term xs) then
                            ((-1.0)*getVal(term xs), getEndStr(term xs), True)
                        else error ("parse error: expression parsing failed " ++ getEndStr(term xs))
                    else failParse s                           
                                                               
------------------------------------------------------------
term [] = failParse []
term s  = (minus_term >| parentheses >| unary_func >| binary_func >| number >| const_p) s
          
------------------------------------------------------------
term2 s = if isSucceed (muldiv_expr s) then muldiv_expr s
            else term s
        
------------------------------------------------------------
muldiv_expr s = let (v1, s1, r1) = term s
                    (v2, s2, r2) = term2 (drop 1 s1)
                in if r1 && r2 
                        then case head s1 of
                                '*' -> (v1*v2, s2, True)
                                '/' -> (v1/v2, s2, True)
                                otherwise -> failParse s1
                        else if r1 then failParse s2 else failParse s1
                        
------------------------------------------------------------               
addsub_expr s = let (v1, s1, r1) = term2 s
                    (v2, s2, r2) = expr (drop 1 s1)
                in if r1 && r2
                        then case head s1 of
                                '+' -> (v1 + v2, s2, True)
                                '-' -> (v1 - v2, s2, True)
                                otherwise -> failParse s1
                        else if r1 then failParse s2 else failParse s1
                        
------------------------------------------------------------
find_table_element s table df = let sl = map (\x -> (x, check_str (fst x) s)) table
                                    found = filter (fst.snd) sl
                                        in case found of
                                        [] -> (df, [], False)
                                        otherwise -> ((snd.fst.head) found, (snd.snd.head) found, True)
                                        
------------------------------------------------------------
const_p s = find_table_element s constants 0.0
                                        
------------------------------------------------------------
get_unary_func s = find_table_element s unary_functions (\_->0)
get_binary_func s = find_table_element s binary_functions (\_ _->0)
                                                                                        
------------------------------------------------------------                                                   
unary_func s = let (f, s1, r) = get_unary_func s
                   (var, s2, r2) = expr (drop 1 s1) 
                in if r && r2 && head s1 == '(' && head s2 == ')' 
                        then (f(var), drop 1 s2, True)
                        else failParse s1
                
                
binary_func s = let (f, s1, r) = get_binary_func s
                    (var1, s2, r2) = expr (drop 1 s1)
                    (var2, s3, r3) = expr (drop 1 s2) 
                in if r && r2 && r3 && head s1 == '(' && head s2 == ',' && head s3 == ')' 
                        then (f var1 var2, drop 1 s3, True)
                        else failParse s1
