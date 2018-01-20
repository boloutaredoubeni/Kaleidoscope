// Learn more about F# at http://fsharp.org
open System
open System.IO

module Token =
  let [<Literal>] Plus = "+"
  let [<Literal>] Minus = "-"
  let [<Literal>] Multiply = "*"
  let [<Literal>] LessThan = "<"
  let [<Literal>] LeftParen = "("
  let [<Literal>] RightParen = ")"
  let [<Literal>] Comma = ","
  let [<Literal>] SpecialCharacters = "!%&*+-./<=>@^|~?"

module rec AST = 

  (* expr - Base type for all expression nodes. *)
  type Expr =
    (* variant for numeric literals like "1.0". *)
    | Number of double
    (* variant for a binary operator. *)
    | BinaryOperation of Op: Operator * Lhs: Expr * Rhs: Expr
    (* variant for referencing a variable, like "a". *)
    | Variable of Name: string
    (* variant for function calls. *)
    | Call of FunctionName: string * Arguments: seq<Expr>

    override expr.ToString() =
      match expr with
      | BinaryOperation (Op=op; Lhs=lhs; Rhs=rhs) -> sprintf "%A %A %A" lhs op rhs
      | Number n -> sprintf "%A : Number" n
      | Variable identifier -> sprintf "%s : Variable" identifier 
      | Call(FunctionName=fn;Arguments=args) ->
        let argRepresentation =
          args
          |> Seq.map string
          |> String.concat ", "
        sprintf "Call %s (%s)" fn argRepresentation
  
  type Operator =
    | Plus
    | Minus
    | LessThan
    | Multiply

    member op.Code =
      match op with
      | Plus -> Token.Plus
      | Minus -> Token.Minus
      | LessThan -> Token.LessThan
      | Multiply -> Token.Multiply

    override op.ToString() = op.Code


  (* proto - This type represents the "prototype" for a function, which captures
   * its name, and its argument names (thus implicitly the number of arguments the
   * function takes). *)
  type Prototype = Prototype of String

  (* func - This type represents a function definition itself. *)
  type Function = Function of Prototype: Prototype * Body: Expr

module Lexer =
  open FParsec
  open Token
  
  let parseLeftParen = pstring LeftParen

  let parseRightParen = pstring RightParen

  let parseComma = pstring Comma
  let isSymbolicOperatorChar = isAnyOf SpecialCharacters
  let remainingOperatorCharsAndWhiteSpace = manySatisfy isSymbolicOperatorChar .>> spaces

  let parseWhiteSpace = spaces

  let parseBetweenBrackets = between parseLeftParen parseRightParen

module Parser =
  open FParsec
  open AST
  open Lexer

  type private AST.Operator with
  
    member op.Precedence =
      match op with
      | Plus -> 20
      | LessThan -> 10
      | Minus -> 30
      | Multiply -> 40

    member op.Associativity =
      match op with
      | Plus -> Associativity.Left
      | _ -> Associativity.Left

  type UserState = unit

  let parseExpr, parseExprRef = createParserForwardedToRef<Expr, _>()

  let parseValue =

    let parseNumber : Parser<Expr, UserState> = pfloat .>> spaces |>> Number

    let parseVariable : Parser<Expr, UserState> = 
      let options = IdentifierOptions()
      let parseIdentifierWithOptions = identifier options
      parseIdentifierWithOptions .>> spaces |>> Variable

    let parseCall =
      let parseArgs =
        let parseExprs = sepBy parseExpr parseComma
        between parseLeftParen parseRightParen parseExprs
      let operator = parseVariable
      operator .>>. parseArgs |>> (fun (fn, args) -> 
        let (Variable name) = fn
        Call(name, args)
      )

    choice [
      parseNumber
      attempt parseCall
      attempt parseVariable
    ]
  
  let addSymbolicInfixOperators (op : Operator) opp =
    let operator = 
      InfixOperator(
        op.Code,
        remainingOperatorCharsAndWhiteSpace,
        op.Precedence,
        op.Associativity,
        (),
        fun _ lhs rhs ->
          BinaryOperation(op, lhs, rhs)
    )
    (opp : OperatorPrecedenceParser<_, _, _>).AddOperator(operator)
  
  let operatorPrecendenceParser = 
    let parseTerm = 
      parseValue .>> parseWhiteSpace
      <|> parseBetweenBrackets parseExpr
    let opp = OperatorPrecedenceParser()
    do 
      opp.TermParser <- parseTerm
      addSymbolicInfixOperators LessThan opp
      addSymbolicInfixOperators Plus opp
      addSymbolicInfixOperators Minus opp
      addSymbolicInfixOperators Multiply opp
    opp

  let parseBinaryOperation = operatorPrecendenceParser.ExpressionParser

  do parseExprRef := parseBinaryOperation
       
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

  let run = runParserOnString parseExpr () "" 

module Driver =

  let [<Literal>] Preamble = "Kaleidoscope in F# and LLVM\n"


  // Read user input until an empty line with a return
  // TODO: pass in a reader or see if fparsec has stdin support
  let readLines scanner =
    let rec readLinesIntoSeqAsync lines =
      async {
        let! nextLine = (scanner: TextReader).ReadLineAsync() |> Async.AwaitTask
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

let tests () = 
  [
    "1.0"
    "variable"
    "45 + 3.0"
    "s < 3"
    "foo(y, 4.0 + z)"
  ] 
  |> Seq.iter (fun program -> Driver.run (new StringReader(program)))


[<EntryPoint>]
let main _ = 
  do 
    let module' = LLVM.ModuleCreateWithName(``my-cool-jit``)
    let builder = LLVM.CreateBuilder()

    (* Run the main "interpreter loop" now. *)
    // tests ()
    Driver.run Console.In
    LLVM.DumpModule(module')
  0