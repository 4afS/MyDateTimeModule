import DateTime

main = do
        putStrLn $ showDT (Time 10 20)
        putStrLn . showDT $ Time 10 30 <+> Time 5 40
        putStrLn . showDT $ Time 5 40 <-> Time 24 50
        print $ toTime "2:48"
        print $ toTime "24:60"
