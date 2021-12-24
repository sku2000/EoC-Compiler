import RvarInterp (interp)
import Rvar (program)
import RvarUniquifyPass (uniquify)
import RemoveComplexOpera (removeComplexOperands)
import ExplicateControl (explicateControlProgram) 

assert :: Bool -> String -> String -> IO ()
assert test passStatement failStatement = if test
    then putStrLn passStatement
    else putStrLn failStatement

-- test Rvar interpreter
testInterp :: IO ()
testInterp = let eval = interp . program in do
                 putStrLn "Running tests Interpreter....."
                 eval "42" >>= \x -> assert (x == 42) "passed '42'" "FAIL '42'"
                 eval "(+ 20 22)" >>= \x -> assert (x == 42) "passed '42'" "FAIL '42'"
                 eval "(let ([x 41]) (+ x 1))" >>= \x -> assert (x == 42) "passed '42'" "FAIL '42'"
    
-- test uniquify pass 

-- test Remove Complex Operands pass
--  (+ (+ 2 3) (let ([x 2]) x))                     
-- (+ (+ 2 3) (let ([x 2]) (+ x (let ([x 9]) x))))
-- (+ (+ 2 3) (let ([x 2]) (+ (- x) (let ([x 9]) x))))
-- (+ (+ 2 3) (let ([x 2]) (+ (+ x x) (let ([x 9]) x))))

                       