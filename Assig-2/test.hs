import RvarInterp (interp)

assert :: Bool -> String -> String -> IO ()
assert test passStatement failStatement = if test
    then putStrLn passStatement
    else putStrLn failStatement

-- test Interpreter

testInterp = do
    putStrLn "Running tests Interpreter....."
    interp "42" >>= \x -> assert (x == 42) "passed '42'" "FAIL '42'"
    interp "(+ 20 22)" >>= \x -> assert (x == 42) "passed '42'" "FAIL '42'"
    interp "(let ([x 41]) (+ x 1))" >>= \x -> assert (x == 42) "passed '42'" "FAIL '42'"
    
