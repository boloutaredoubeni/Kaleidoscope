// Learn more about F# at http://fsharp.org
open System
open System.IO

module AST = 

  (* expr - Base type for all expression nodes. *)
  type Expr =
    (* variant for numeric literals like "1.0". *)
    | Number of double
    (* variant for a binary operator. *)
    | BinaryOperation of Op: string * Lhs: Expr * Rhs: Expr
    (* variant for referencing a variable, like "a". *)
    | Variable of Name: string
    (* variant for function calls. *)
    | Call of FunctionName: string * Arguments: seq<Expr>

    override expr.ToString() =
      match expr with
      | BinaryOperation (Op=op; Lhs=lhs; Rhs=rhs) -> sprintf "%A %A %A" lhs op rhs
      | Number n -> sprintf "%A : Number" n
      | _ -> string expr

  (* proto - This type represents the "prototype" for a function, which captures
   * its name, and its argument names (thus implicitly the number of arguments the
   * function takes). *)
  type Prototype = Prototype of String

  (* func - This type represents a function definition itself. *)
  type Function = Function of Prototype: Prototype * Body: Expr

module Parser =
  open FParsec
  open AST

  type UserState = unit

  let parseNumber : Parser<Expr, UserState> = pfloat |>> Number

  let isSymbolicOperatorChar = isAnyOf "!%&*+-./<=>@^|~?"
  let remainingOperatorCharsAndWhiteSpace = manySatisfy isSymbolicOperatorChar .>> spaces
  
  let addSymbolicInfixOperators prefix precendence associativity opp =
    let operator = 
      InfixOperator(
        prefix,
        remainingOperatorCharsAndWhiteSpace,
        precendence,
        associativity,
        (),
        fun remainingOperatorChars lhs rhs ->
          BinaryOperation(prefix + remainingOperatorChars, lhs, rhs)
    )
    (opp : OperatorPrecedenceParser<_, _, _>).AddOperator(operator)
  
  let operatorPrecendenceParser = 
    let opp = OperatorPrecedenceParser()
    do 
      opp.TermParser <- pfloat .>> spaces |>> Number
      addSymbolicInfixOperators "<" 10 Associativity.Left opp
      addSymbolicInfixOperators "+" 20 Associativity.Left opp
      addSymbolicInfixOperators "-" 30 Associativity.Left opp
      addSymbolicInfixOperators "*" 40 Associativity.Left opp
    opp
  

  let parseBinaryOperation = operatorPrecendenceParser.ExpressionParser

  let parsePrimary : Parser<Expr, _> = parseNumber

  let parser =
    choice [
      parseBinaryOperation
      // parsePrimary
    ]
  let showResult = 
    function
    | Success(result, _, _) ->
      do
        printfn ""
        printfn "=> %s" (string result)
        printfn ""
    | Failure(message, _, _) ->
      do 
        printfn ""
        printfn "☠️ %s" message
        printfn ""

  let run = runParserOnString parser () "" 

module Driver =

  let [<Literal>] Preamble = "Kaleidoscope in F# and LLVM\n"


  // Read user input until an empty line with a return
  // TODO: pass in a reader or see if fparsec has stdin support
  let readLines (scanner: TextReader) =
    let rec readLinesIntoSeqAsync lines =
      // TODO: parse asynchronously and parser in the background
      async {
        let! nextLine = scanner.ReadLineAsync() |> Async.AwaitTask
        if String.IsNullOrEmpty(nextLine) 
          then return seq lines
          else
            do printf "-\t"
            return! readLinesIntoSeqAsync (lines @ [ nextLine ])
      } 
        
    seq { 
      yield! readLinesIntoSeqAsync [] 
        |> Async.RunSynchronously
        |> Seq.rev 
    } |> String.concat "\n"

  // Main entrypoint for the driver  
  let run scanner =
    do 
      printfn "%s" Preamble
      let prompt = "ready>\t"
      let rec runRepl () =
        do 
          printf "%s" prompt
          let lines = readLines scanner
          if String.IsNullOrWhiteSpace(lines)
            then runRepl ()
          let result = Parser.run lines
          Parser.showResult result
          runRepl ()
      runRepl ()

open LLVMSharp

let [<Literal>] ``my-cool-jit`` = "my cool jit"

[<EntryPoint>]
let main _ = 
  do 
    let module' = LLVM.ModuleCreateWithName(``my-cool-jit``)
    let builder = LLVM.CreateBuilder()

    (* Run the main "interpreter loop" now. *)
    Driver.run Console.In
    LLVM.DumpModule(module')
  0