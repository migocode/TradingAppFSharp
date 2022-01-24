[<EntryPoint>]
let main argv =
    printfn "Welcome to the 'Trading App 2000'"
    printfn "Please enter your commands to interact with the system."
    printfn "Press CTRL+C to stop the program."
    printf "> "

    let initialState = State.init ()
    Repl.loop initialState
    0