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

  let [<Literal>] Def = "def"
  let [<Literal>] Extern = "extern"

module rec AST = 

  (* expr - Base type for all expression nodes. *)
  type Expr =
    (* variant for numeric literals like "1.0". *)
    | Number of double
    (* variant for a binary operator. *)
    | BinaryOperation of (Operator * Expr * Expr)
    (* variant for referencing a variable, like "a". *)
    | Variable of Name: string
    (* variant for function calls. *)
    | Call of (string * seq<Expr>)

    override expr.ToString() =
      match expr with
      | BinaryOperation (op, lhs, rhs) -> sprintf "%s %s %s" (lhs.ToString()) (string op) (rhs.ToString())
      | Number n -> sprintf "%f" n
      | Variable identifier -> sprintf "%s" identifier 
      | Call(fn, args) ->
        let argRepresentation =
          args
          |> Seq.map string
          |> String.concat ", "
        sprintf "%s (%s)" fn argRepresentation
  
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

  type Prototype = string * seq<string>

  type Decl =
  (* proto - This type represents the "prototype" for a function, which captures
   * its name, and its argument names (thus implicitly the number of arguments the
   * function takes). *)
    | Prototype of Prototype

  (* func - This type represents a function definition itself. *)
    | Function of Prototype: Prototype * Body: Expr

    override decl.ToString() = 
      let prototypeToString (name, args) = 
        let argRepresentation = String.concat " " args
        sprintf "%s %s" name argRepresentation
      match decl with
      | Prototype (name, args) -> prototypeToString (name, args)
      | Function (prototype, body) ->
        sprintf "%s %s" (prototypeToString prototype) (body.ToString())

module Lexer =
  open FParsec
  open Token

  let parseWhiteSpace = spaces
  
  let parseLeftParen = pstring LeftParen .>> parseWhiteSpace

  let parseRightParen = pstring RightParen .>> parseWhiteSpace

  let parseComma = pstring Comma .>> parseWhiteSpace
  let isSymbolicOperatorChar = isAnyOf SpecialCharacters


  let remainingOperatorCharsAndWhiteSpace = manySatisfy isSymbolicOperatorChar .>> parseWhiteSpace

  let parseWhiteSpace1 = spaces1

  let parseBetweenBrackets = between parseLeftParen parseRightParen

  let parseDef = skipString Def .>> parseWhiteSpace

  let parseExtern = skipString Extern .>> parseWhiteSpace

module Parser =
  open FParsec
  open AST
  open Lexer

  type UserState = unit

  [<RequireQualifiedAccess>]
  module private Expr =
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

    let private parseExpr, parseExprRef = createParserForwardedToRef<Expr, _>()

    let parseIdentifier = 
      let options = IdentifierOptions()
      let parseIdentifierWithOptions = identifier options
      parseIdentifierWithOptions .>> parseWhiteSpace

    let private parseValue =

      let parseNumber : Parser<Expr, UserState> = pfloat .>> spaces |>> Number

      let parseVariable = parseIdentifier |>> Variable

      let parseCall =
        let parseArgs =
          let parseExprs = sepBy parseExpr parseComma
          let parseArguments = between parseLeftParen parseRightParen parseExprs
          parseArguments |>> seq
        let operator = parseIdentifier
        operator .>>. parseArgs |>> Call

      choice [
        parseNumber
        attempt parseCall
        attempt parseVariable
      ]
  
    let private addSymbolicInfixOperators (op : Operator) opp =
      let operator = 
        InfixOperator(
          op.Code,
          remainingOperatorCharsAndWhiteSpace,
          op.Precedence,
          op.Associativity,
          (),
          fun _ lhs rhs -> BinaryOperation(op, lhs, rhs)
      )
      (opp : OperatorPrecedenceParser<_, _, _>).AddOperator(operator)
    
    let private operatorPrecendenceParser = 
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

    let private parseBinaryOperation = operatorPrecendenceParser.ExpressionParser

    do parseExprRef := parseBinaryOperation

    let parse = parseWhiteSpace >>. parseExpr
         
  [<RequireQualifiedAccess>]
  module private Decl =

    type private Decl with
      static member EmptyPrototype = ("", Seq.empty)

    let private parseTopLevel =
      let parsePrototype = 
        let parseArgument = sepBy Expr.parseIdentifier parseWhiteSpace
        let parseArguments = between parseLeftParen parseRightParen parseArgument
        Expr.parseIdentifier .>> parseWhiteSpace .>>. parseArguments |>> (fun (name, args) -> (name, seq args))
      let parseDefinition = 
        parseDef .>> parseWhiteSpace >>. parsePrototype .>>. Expr.parse |>> Function
      let parseExtern =
        parseExtern .>> parseWhiteSpace >>. parsePrototype |>> Prototype
      let parseTopLevel =
        Expr.parse |>> (fun body -> Function (Decl.EmptyPrototype, body))

      choice [
        parseDefinition
        attempt parseExtern
        attempt parseTopLevel
      ]

    let parse = parseTopLevel 
  
  let parse = parseWhiteSpace >>. Decl.parse

  let run = runParserOnString parse () "" 

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
open FParsec

let [<Literal>] ``my-cool-jit`` = "my cool jit"

let tests () = 
  [
    // "def foo(x y) x+foo(y, 4.0);"
    // "def foo(x y) x+y y;"
    // "def foo(x y) x+y );"
    "extern sin(a);"
  ]
  |> Seq.map (fun program ->
    async {
      let result = Parser.run program
      match result with
      | Success(tree, _,_) -> printfn "%A" tree
      | Failure(errorMessage, _, _) -> failwith errorMessage
    })
  |> Async.Parallel
  |> Async.RunSynchronously


[<EntryPoint>]
let main _ = 
  do 
    let module' = LLVM.ModuleCreateWithName(``my-cool-jit``)
    let builder = LLVM.CreateBuilder()

    (* Run the main "interpreter loop" now. *)
    tests () |> ignore
    Driver.run Console.In
    LLVM.DumpModule(module')
  0