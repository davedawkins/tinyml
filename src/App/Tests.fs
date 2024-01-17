module Tests

open Types

let debugParser = false

let testSrc = @"
let rec fac = fun n -> if n then 1 else n * fac (n - 1) in fac 5
"

type TestResult = 
    | ResultValue of obj
    | ResultAst of obj
    | ResultRunError
    | ResultCompileError

let fmt ( (a,b,c) : CompilerMessage) =
    sprintf "%A: %s" a b


open Fable.Core
[<Emit("$0 == $1")>]
let jsEquals(a,b) = jsNative

type TestCfg( name : string, src : string, expected : TestResult ) =
    let success() =
        Log.log( sprintf "%s: Success" name )

    let error(msg : string) =
        Log.log( sprintf "%s: Failed: %s" name msg )

    let errorExpected() =
        Log.log( sprintf "%s: Success with expected error" name )

    do ()
    member _.Run( compiler : ICompiler ) =
        promise {
            match compiler.Compile(name,src) with
            | Ok exe ->

                match expected with
                | ResultValue expectedValue ->

                    match exe.Run() with
                    | Ok v -> 
                        if not (jsEquals(expectedValue, v)) then
                            error (sprintf "Expected '%A', got '%A'" expectedValue v)
                        else
                            success()

                    | Error s ->
                        match expected with
                        | ResultRunError ->
                            errorExpected()
                        | _ ->
                            error s
                | ResultAst expectedAst ->
                    error ("AST result not supported yet")
                
                | _ ->
                    error (sprintf "Expected %A" expected)

            | Result.Error es ->
                match expected with
                | ResultCompileError ->
                    errorExpected()
                | _ ->
                    error (es |> List.map fmt |> String.concat "\n")

                failwith "Compilation failure"
            return ()
        }

let isEmpty (s: string) = System.String.IsNullOrWhiteSpace(s)
let startsWith (s: string) (prefix : string) = s.StartsWith(prefix)

let makeTests ( cfg : string ) =
    let mutable tests = []
    let mutable state = 0

    let mutable name = ""
    let mutable startLine = -1
    let mutable src = ""
    let mutable resultType = ""
    let mutable resultValue = ""

    let buildTest () =
        if debugParser then sprintf "Parsing: %s, %s, %s" name resultType resultValue |> Log.log

        let r =
            match resultType.ToLower() with
            | "value" -> ResultValue (Fable.Core.JS.JSON.parse (resultValue.Trim()))
            | "runerror" -> ResultRunError
            | "compileerror" -> ResultCompileError
            | "ast" -> ResultAst (Fable.Core.JS.JSON.parse (resultValue.Trim()))
            | x -> failwith (sprintf "%s:%d: Unsupported result type: %s" name startLine x)

        tests <- tests @ [ TestCfg( name, src, r) ]

        if debugParser then Log.log( "Parsed test " + name )

        name <- ""
        src <- ""
        resultType <- ""
        resultValue <- ""

    let rec processLine (lineNo : int) (line : string) =
        if debugParser then sprintf "%d:%d:%s" lineNo state line |> Log.log

        match state with 

        // -- State 0: Reading test name ------------------
        | 0 when isEmpty line -> 
            ()

        | 0 when startsWith line "#test" ->
            startLine <- lineNo + 1
            name <- line.Substring(5).Trim()
            if debugParser then Log.log("Found test: " + name)
            if name = "" then name <- "Test" + (string (tests.Length + 1))
            state <- 1

        | 0 -> 
            failwith "Expected #test <name>"

        | 1 when startsWith line "#" ->
            let tokens = line.Substring(1).Split([|' '|], 2)
            resultType <- tokens[0]
            resultValue <- if tokens.Length > 1 then tokens[1] else ""
            if debugParser then Log.log(sprintf "Found resultType %s, value %s" resultType resultValue)
            state <- 2

        | 1 ->
            src <- src + line + "\n"

        | 2 when (startsWith line "#") ->
            buildTest()
            state <- 0
            processLine lineNo line

        | 2 ->
            resultValue <- resultValue + line + "\n"

        | _ -> failwith "Illegal state"


    try 
        cfg.Split( '\n' ) |> Array.iteri processLine
        if state = 2 then buildTest()
    with
    | x -> Log.log(sprintf "Exception: %s" x.Message)

    tests

type Assert() =
    static Fail( expected : 'a, actual : 'a, label : string ) =
        failwith <| sprintf "Expected %A, got %A: %s" expected actual label

    static AreEqual( expected : int, actual : int, label : string ) =
        if expected <> actual then Assert.Fail (expected,actual,label)

let runTests( compiler : ICompiler) (cfg : string ) =
    let tests = makeTests cfg
    
    Log.log("-- RUNNING TESTS -----------------------------------------")
    promise {
        for test in tests do

            do! test.Run( compiler )        
    }

    // promise {
    //     try
    //         let suite = Tests(compiler)
    //         do! suite.test1()
    //         do! suite.test2()
    //         Log.log ("TESTS SUCCEEDED!")
    //     with
    //     | x -> 
    //         Log.log ("TESTS FAILED: " + x.Message)
    // }
