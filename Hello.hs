module Hello where
    
helloWorld :: String
helloWorld = hello "world"
             
hello :: String -> String
hello s = "hello " ++ s
