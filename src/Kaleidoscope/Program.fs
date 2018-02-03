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
      | BinaryOperation (op, lhs, rhs) -> sprintf "%O %O %O" lhs op rhs
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
        sprintf "%s %O" (prototypeToString prototype) body

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
        printfn "=> %O" result
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
          if String.IsNullOrWhiteSpace(lines) then runRepl ()
          let result = Parser.run lines
          Parser.showResult result
          runRepl ()
      runRepl ()

open LLVMSharp
open FParsec
open System.Reflection.Metadata.Ecma335

let [<Literal>] JitName = "my cool jit"

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

let context = LLVM.GetGlobalContext()
let module' = LLVM.ModuleCreateWithNameInContext(JitName, context)
let builder = LLVM.CreateBuilderInContext(context)
let engine = ref (LLVMExecutionEngineRef(IntPtr.Zero))

module Codegen =

  open AST
  open LLVMSharp

  let rec codegenExpr (namedValues: Map<string, LLVMValueRef>) =
    function
    | Number n -> LLVM.ConstReal(LLVM.DoubleType(), n)
    | Variable name ->
      match namedValues.TryFind(name) with
      | Some llvmValue -> llvmValue
      | _ -> failwithf "unknown variable %s" name
    | BinaryOperation (op, lhs, rhs) -> 
      let lhs = codegenExpr namedValues lhs
      let rhs = codegenExpr namedValues rhs
      match op with
      | Plus -> LLVM.BuildAdd(builder, lhs, rhs, "addtmp")
      | Minus -> LLVM.BuildSub(builder, lhs, rhs, "subtmp")
      | Multiply -> LLVM.BuildMul(builder, lhs, rhs, "multmp")
      | LessThan ->
        LLVM.BuildUIToFP(builder, LLVM.BuildFCmp(builder, LLVMRealPredicate.LLVMRealULT, lhs, rhs, "cmptmp"), LLVM.DoubleType(), "booltmp");
    | Call (functionName, arguments) ->
      let calleeF = 
        let calleeF = LLVM.GetNamedFunction(module', functionName)
        if calleeF.Pointer <> IntPtr.Zero then failwithf "Unknown function %s referenced" functionName
        let argumentCount = (uint32 << Seq.length) arguments
        if LLVM.CountParams(calleeF) <> argumentCount then failwith "Incorrect # of arguments passed"
        calleeF
      let argsV = 
        let n = Math.Max((Seq.length arguments), 1)
        let argsV = Array.zeroCreate n
        let argsV = Seq.map2 (fun expr _valueRef -> codegenExpr namedValues expr) arguments argsV
        Seq.toArray argsV
      LLVM.BuildCall(builder, calleeF, argsV, "calltmp")

  let rec codegenDecl (namedValues: Map<string, LLVMValueRef>) =
    function
    | Prototype (name, arguments) -> 
      let argumentCount = (uint32 << Seq.length) arguments
      let mutable function' = LLVM.GetNamedFunction(module', name)
      if (function'.Pointer <> IntPtr.Zero)
        then 
          if LLVM.CountBasicBlocks(function') <> 0u then failwithf "redefinition of function %s" name
          if LLVM.CountParams(function') <> argumentCount then failwithf "redefinition of function %s with a different # of args" name
        else 
          let arguments =
            let n = Math.Max(int argumentCount, 1)
            Array.create n (LLVM.DoubleType())
          function' <-
            let functionType = LLVM.FunctionType(LLVM.DoubleType(), arguments, false) 
            LLVM.AddFunction(module', name, functionType)
          LLVM.SetLinkage(function', LLVMLinkage.LLVMExternalLinkage)
      let mutable namedValues = namedValues
      Seq.iteri (fun i argumentName -> 
        let param = LLVM.GetParam(function', uint32 i)
        LLVM.SetValueName(param, argumentName)
        namedValues <- namedValues.Add(argumentName, param)
      ) arguments
      function', namedValues
    | Function (proto, body) ->
      let namedValues = Map.empty
      let (function', namedValues) = codegenDecl namedValues (Prototype proto)
      LLVM.PositionBuilderAtEnd(builder, LLVM.AppendBasicBlock(function', "entry"))
      let body = 
        try
          codegenExpr namedValues body
        with _ ->
          LLVM.DeleteFunction(function')
          reraise ()
      LLVM.BuildRet(builder, body) |> ignore
      LLVM.VerifyFunction(function', LLVMVerifierFailureAction.LLVMPrintMessageAction) |> ignore
      function', namedValues

[<EntryPoint>]
let main _ = 
  do 
    LLVM.LinkInMCJIT()
    LLVM.InitializeX86TargetInfo()
    LLVM.InitializeX86Target()
    LLVM.InitializeX86TargetMC()
    LLVM.InitializeX86AsmParser()
    LLVM.InitializeX86AsmPrinter()
  let mutable message = ""
  if 1 = (LLVM.CreateExecutionEngineForModule(engine, module', &message)).Value 
    then 
      printfn "%s" message
      1
    else
      let passManager = LLVM.CreateFunctionPassManagerForModule(module')
      // Provide basic AliasAnalysis support for GVN.
      LLVM.AddBasicAliasAnalysisPass(passManager)

      // Promote allocas to registers.
      LLVM.AddPromoteMemoryToRegisterPass(passManager)

      // Do simple "peephole" optimizations and bit-twiddling optzns.
      LLVM.AddInstructionCombiningPass(passManager)

      // Reassociate expressions.
      LLVM.AddReassociatePass(passManager)

      // Eliminate Common SubExpressions.
      LLVM.AddGVNPass(passManager)

      // Simplify the control flow graph (deleting unreachable blocks, etc).
      LLVM.AddCFGSimplificationPass(passManager)

      ignore <| LLVM.InitializeFunctionPassManager(passManager)
      (* Run the main "interpreter loop" now. *)
      ignore <| tests ()
      Driver.run(Console.In)
      LLVM.DumpModule(module')
      LLVM.DisposeModule(module')
      LLVM.DisposeExecutionEngine(!engine)
      LLVM.DisposePassManager(passManager)
      0
