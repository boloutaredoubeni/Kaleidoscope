module Kaleidoscope

open System
open System.IO
open System.Collections.Generic
open System.Text
open System.Xml.Xsl


[<RequireQualifiedAccess>]
module internal Dictionary =

    let tryFind key (dictionary: IDictionary<_, _>) =
        let (found, value) = dictionary.TryGetValue(key)
        if found
            then Some value
            else None

[<AutoOpen>]
module internal ImplicitOperators =
    let inline (!>) (x:^a) : ^b = ((^a or ^b) : (static member op_Implicit : ^a -> ^b) x)

type ResultBuilder() =
    member __.Bind(result, action) = Result.bind action result
    member __.Return ok = Result.Ok ok
    member __.ReturnFrom result = result
    member __.Zero () = Result.Ok ()

let resultOf = ResultBuilder()

module Result =

    let tryWith action =
        try Result.Ok (action ())
        with e -> Result.Error e

    let throw result = Result.mapError raise result |> ignore
    
    let iter f =
        function
        | Result.Ok t -> f t
        | _ -> ()

    let rec traverseSeq f xs =
        let folder head tail = 
            resultOf {
                let! head = f head
                let! tail = tail
                return seq { yield head; yield! tail }
            }
        Seq.foldBack folder xs (Result.Ok Seq.empty)
    
    let all result = traverseSeq id result

module internal Token =
    let [<Literal>] Plus = "+"
    let [<Literal>] Minus = "-"
    let [<Literal>] Multiply = "*"
    let [<Literal>] LessThan = "<"
    let [<Literal>] LeftParen = "("
    let [<Literal>] RightParen = ")"
    let [<Literal>] Comma = ","
    let [<Literal>] Equal = "="
    let [<Literal>] SpecialCharacters = "!%&*+-./<=>@^|~?"
    let [<Literal>] Def = "def"
    let [<Literal>] Extern = "extern"
    let [<Literal>] For = "for"
    let [<Literal>] If = "if"
    let [<Literal>] Then = "then"
    let [<Literal>] Else = "else"
    let [<Literal>] In = "in"
    let [<Literal>] Binary = "binary"
    let [<Literal>] Unary = "unary"
    let [<Literal>] Var = "var"
    let [<Literal>] Semicolon = ";"
    let [<Literal>] Hash = "#"

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
        /// if/then/else
        | If of  (Expr * Expr * option<Expr>)
        /// for/in
        | For of string * Expr * Expr * option<Expr> * Expr
        /// var <identifier> = <expr>
        | Assign of Vars: seq<(Expr * Expr option)> * Body: Expr

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
            | _ -> ""

    type Operator =
        | Plus
        | Minus
        | LessThan
        | Multiply
        | Equal

        member op.Code =
            match op with
            | Plus -> Token.Plus
            | Minus -> Token.Minus
            | LessThan -> Token.LessThan
            | Multiply -> Token.Multiply
            | Equal -> Token.Equal

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

    let parseSepByComma expr = sepBy expr parseComma

    let parseDef = skipString Def .>> parseWhiteSpace1

    let parseExtern = skipString Extern .>> parseWhiteSpace1

    let parseFor = skipString For .>> parseWhiteSpace1

    let parseIn = skipString In .>> parseWhiteSpace1

    let parseIf = skipString If .>> parseWhiteSpace1

    let parseThen = skipString Then .>> parseWhiteSpace1

    let parseElse = skipString Else .>> parseWhiteSpace1

    let parseEqual = skipString Equal .>> parseWhiteSpace

    let parseVar = skipString Var .>> parseWhiteSpace1

module Parser =
    open FParsec
    open AST
    open Lexer

    type private UserState = unit


    [<RequireQualifiedAccess>]
    module private Expr =
        type private AST.Operator with

            member op.Precedence =
                match op with
                | Plus -> 20
                | LessThan -> 10
                | Minus -> 30
                | Multiply -> 40
                | Equal -> 2

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

            let parseAssignBody = parseEqual .>> parseWhiteSpace >>. parseExpr

            let parseAssign =
                pipe2 parseIdentifier parseAssignBody (fun var expr -> (var, expr))

            let parseAssignExprs =
                let parseInnerExpr = parse {
                    let! var = parseVariable
                    do! parseEqual
                    let! body = parseExpr |> opt
                    return (var, body)
                  }
                parse {
                  do! parseVar
                  let! defs = parseSepByComma parseInnerExpr |>> Seq.ofList
                  do! parseIn
                  let! body = parseExpr
                  return Assign (defs, body)
                }
            //parseVar >>. parseAssign |>> (fun (var,  expr) -> Assign (Variable var, expr))

            let parseCall =
                parse {
                  let! args =
                    let parseExprs = sepBy parseExpr parseComma
                    let parseArguments = between parseLeftParen parseRightParen parseExprs
                    parseArguments |>> seq
                  let! operator = parseIdentifier
                  return Call(operator, args)
                }

            let parseIfExpr =
                parse {
                  let! if' = parseIf >>. parseExpr .>> parseWhiteSpace1
                  let! then' = parseThen >>. parseExpr .>> parseWhiteSpace1
                  let! else' = parseElse >>. opt parseExpr .>> parseWhiteSpace1
                  return If (if', then', else')
                }


            let parseForExpr =
                parse {
                  let! (id, assign) = parseFor >>. parseAssign
                  let! condition = parseComma >>. parseExpr .>> parseWhiteSpace
                  let! increment = opt (parseComma >>. parseExpr .>> parseWhiteSpace)
                  let! body = parseIn >>. parseExpr
                  return For (id, assign, condition, increment, body)
                }

            choice [
                parseNumber
                attempt parseCall
                attempt parseVariable
                attempt parseIfExpr
                attempt parseForExpr
                attempt parseAssignExprs
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

    let private parse = parseWhiteSpace >>. Decl.parse

    let private run = runParserOnString parse () ""

    exception ParserException of (string * ParserError)

    type IParser =
        abstract member RunOnString: input:string -> Result<Decl, exn>
        abstract member RunOnStream: iostream:Stream -> Result<Decl, exn>
        abstract member RunOnFilePath: path:string -> Result<Decl, exn>


    let Create () =
        let parseResultToFSResult =
            function
            | ParserResult.Success (t, _, _) -> Result.Ok t
            | Failure (msg, parserError, _) -> Result.Error (ParserException (msg, parserError))
        { new IParser with
            member __.RunOnString input = runParserOnString parse () "" input |> parseResultToFSResult
            member __.RunOnFilePath path = runParserOnFile parse () path Encoding.UTF8  |> parseResultToFSResult
            member __.RunOnStream stream = runParserOnStream parse () "" stream Encoding.UTF8 |> parseResultToFSResult
        }

open LLVMSharp

module Codegen =

    open AST

    exception CodeGenerationException of string

    let private (|PrototypeArgs|_|) =
        function
        | Prototype (_, args) -> Some args
        | _ -> Some Seq.empty

    type ICodeGenerator =
        abstract member Run: Decl -> unit

    let Create module' builder context =
        let namedValues = dict []
        let functionProtos = dict []
        let getFunction functionName =
            let f = LLVM.GetNamedFunction(module', functionName)
            if f.Pointer <> IntPtr.Zero
              then Some f
              else Dictionary.tryFind functionName functionProtos
        let createEntryBlockAlloca function' varName =
            use builder = new IRBuilder(context)
            builder.PositionBuilder((!>) function', function')
            builder.CreateAlloca(LLVM.DoubleType(), varName)
        let createArgumentAllocas function' prototype =
            let args =
              match prototype with
              | PrototypeArgs args -> args
            Seq.iteri (fun i ai ->
              let varName = (args |> Seq.toArray).[i]
              let alloca = createEntryBlockAlloca function' varName
              do
                LLVM.BuildStore(builder, ai, alloca) |> ignore
                namedValues.Add(varName, alloca)
            ) (LLVM.GetParams(function'))
        let rec codegenExpr expr =
            resultOf {
                match expr with
                | Number n -> return LLVM.ConstReal(LLVM.DoubleType(), n)
                | Variable name ->
                    match Dictionary.tryFind name namedValues with
                    | Some llvmValue -> return LLVM.BuildLoad(builder, llvmValue, name)
                    | _ -> return! Result.Error (CodeGenerationException <| sprintf "unknown variable %s" name)
                | BinaryOperation (op, lhs, rhs) ->
                    let! lhs = codegenExpr lhs
                    let! rhs = codegenExpr rhs
                    return 
                        match op with
                        | Plus -> LLVM.BuildAdd(builder, lhs, rhs, "addtmp")
                        | Minus -> LLVM.BuildSub(builder, lhs, rhs, "subtmp")
                        | Multiply -> LLVM.BuildMul(builder, lhs, rhs, "multmp")
                        | LessThan ->
                            LLVM.BuildUIToFP(
                                builder, 
                                LLVM.BuildFCmp(builder, LLVMRealPredicate.LLVMRealULT, lhs, rhs, "cmptmp"), 
                                LLVM.DoubleType(), 
                                "booltmp"
                            )
                | Call (functionName, arguments) ->
                    let calleeF =
                        let calleeF = LLVM.GetNamedFunction(module', functionName)
                        if calleeF.Pointer <> IntPtr.Zero then failwithf "Unknown function %s referenced" functionName
                        let argumentCount = (uint32 << Seq.length) arguments
                        if LLVM.CountParams(calleeF) <> argumentCount then failwith "Incorrect # of arguments passed"
                        calleeF
                    let! argsV =
                        let n = Math.Max((Seq.length arguments), 1)
                        let argsV = Array.zeroCreate n
                        let argsV = Seq.map2 (fun expr _valueRef -> codegenExpr expr) arguments argsV
                        let argsV = Result.all argsV
                        Result.map Seq.toArray argsV
                    return LLVM.BuildCall(builder, calleeF, argsV, "calltmp")
                | If (condition, thenExpr, elseExpr) ->
                    let! condv =
                        resultOf {
                            let! condition = codegenExpr condition
                            return LLVM.BuildFCmp(
                              builder,
                              LLVMRealPredicate.LLVMRealONE,
                              condition,
                              LLVM.ConstReal(LLVM.DoubleType(), 0.0),
                              "ifcond"
                            )
                        }
                    let func = LLVM.GetBasicBlockParent(LLVM.GetInsertBlock(builder))
                    let mutable thenBB = LLVM.AppendBasicBlock(func, "then")
                    let mutable elseBB = LLVM.AppendBasicBlock(func, "else")
                    let mergeBB = LLVM.AppendBasicBlock(func, "ifcont")
                    LLVM.BuildCondBr(builder, condv, thenBB, elseBB) |> ignore
                    LLVM.PositionBuilderAtEnd(builder, thenBB) |> ignore
                    let! thenV = codegenExpr thenExpr
                    thenBB <-
                        let thenBB = LLVM.GetInsertBlock(builder)
                        LLVM.PositionBuilderAtEnd(builder, elseBB)
                        thenBB
                    let elseV = Option.map codegenExpr elseExpr
                    LLVM.BuildBr(builder, mergeBB) |> ignore
                    do
                        elseBB <-
                            let elseBB = LLVM.GetInsertBlock(builder)
                            LLVM.PositionBuilderAtEnd(builder, mergeBB)
                            elseBB

                    let phi =
                        let phi = LLVM.BuildPhi(builder, LLVM.DoubleType(), "iftmp")
                        LLVM.AddIncoming(phi, [| thenV |], [| thenBB |], 1u)
                        Option.iter
                          (fun elseV -> 
                            resultOf {
                                let! elseV = elseV
                                LLVM.AddIncoming(phi, [| elseV |], [| elseBB |], 1u)
                            } |> Result.throw)
                          elseV
                        phi

                    return phi
                | For (varName, start, finish, step, body) ->
                    // Output this as:
                    //   ...
                    //   start = startexpr
                    //   goto loop
                    // loop:
                    //   variable = phi [start, loopheader], [nextvariable, loopend]
                    //   ...
                    //   bodyexpr
                    //   ...
                    // loopend:
                    //   step = stepexpr
                    //   nextvariable = variable + step
                    //   endcond = endexpr
                    //   br endcond, loop, endloop
                    // outloop:

                    // Emit the start code first, without 'variable' in scope.


                    // Make the new basic block for the loop header, inserting after current
                    // block.
                    let preheaderBB = LLVM.GetInsertBlock(builder)
                    let function' = LLVM.GetBasicBlockParent(preheaderBB)
                    let alloca = createEntryBlockAlloca function' varName
                    let! start = codegenExpr start
                    do LLVM.BuildStore(builder, start, alloca) |> ignore
                    let loopBB = LLVM.AppendBasicBlock(function', "loop")


                    // Insert an explicit fall through from the current block to the LoopBB.
                    LLVM.BuildBr(builder, loopBB) |> ignore

                    // Start insertion in LoopBB.
                    LLVM.PositionBuilderAtEnd(builder, loopBB)

                    // Start the PHI node with an entry for Start.
                    let variable = LLVM.BuildPhi(builder, LLVM.DoubleType(), varName)
                    LLVM.AddIncoming(variable, [| start |], [| preheaderBB |], 1u)

                    let mutable oldVal = None
                    // Within the loop, the variable is defined equal to the PHI node.  If it
                    // shadows an existing variable, we have to restore it, so save it now.

                    do
                        match Dictionary.tryFind varName namedValues with
                        | Some oldVal' -> oldVal <- Some oldVal'
                        | None -> ()
                        namedValues.Add(varName, variable)

                  // Emit the body of the loop.  This, l he value computed by the body, but don't
                  // allow an error.
                    codegenExpr body |> Result.throw
                    let! step =
                        match step with
                        | Some step -> codegenExpr step
                        | None -> LLVM.ConstReal(LLVM.DoubleType(), 1.0) |> Result.Ok

                    let! cond =
                        resultOf {
                            let! cond = codegenExpr finish

                            // Compute the end condition.
                            return LLVM.BuildFCmp(
                                builder, 
                                LLVMRealPredicate.LLVMRealONE, 
                                cond, 
                                LLVM.ConstReal(LLVM.DoubleType(), 0.0), 
                                "loopcond"
                            )
                        }
                    let currentVar = LLVM.BuildLoad(builder, alloca, varName)
                    let nextVar = LLVM.BuildFAdd(builder, currentVar, step, "nextvar")
                    do LLVM.BuildStore(builder, nextVar, alloca) |> ignore
                    // Create the "after loop" block and insert it.
                    let loopEndBB = LLVM.GetInsertBlock(builder)
                    let afterBB = LLVM.AppendBasicBlock(function', "afterloop")

                    // Insert the conditional branch into the end of LoopEndBB.
                    LLVM.BuildCondBr(builder, cond, loopBB, afterBB) |> ignore

                    // Any new code will be inserted in AfterBB.
                    LLVM.PositionBuilderAtEnd(builder, afterBB)

                    // Add a new entry to the PHI node for the backedge.
                    LLVM.AddIncoming(variable, [| nextVar |], [| loopEndBB |], 1u)

                    // Restore the unshadowed variable
                    do Option.iter (fun oldVal -> namedValues.Add(varName, oldVal)) oldVal

                      // Restore the unshadowed variable.
                    return LLVM.ConstNull(LLVM.DoubleType())
                | Assign (variables, body) ->
                  let mutable oldBindings = Seq.empty
                  let function' =
                    let insertionBlock = LLVM.GetInsertBlock(builder)
                    LLVM.GetBasicBlockParent(insertionBlock)
                  do
                    Seq.iter (fun (Variable name, init) ->
                        resultOf {
                          let! initVal =
                            match init with
                            | Some init -> codegenExpr init
                            | None -> LLVM.ConstReal(LLVM.DoubleType(), 0.0) |> Result.Ok
                          let alloca = createEntryBlockAlloca  function' name
                          do
                            LLVM.BuildStore(builder, initVal, alloca) |> ignore
                            Dictionary.tryFind name namedValues
                            |> Option.iter (fun oldValue ->
                              oldBindings <- seq {
                                yield (name, oldValue)
                                yield! oldBindings
                              }
                            )
                          namedValues.Add(name, alloca)
                        } |> Result.throw
                    ) variables

                  let! body = codegenExpr body
                  do Seq.iter (fun (name, oldValue) -> namedValues.Add(name, oldValue)) oldBindings
                  return body
                }
        let rec codegenDecl decl =
            match decl with
            | Prototype (name, arguments) ->
              let argumentCount = (uint32 << Seq.length) arguments
              let mutable function' = LLVM.GetNamedFunction(module', name)
              Result.tryWith (fun () -> 
                if (function'.Pointer <> IntPtr.Zero)
                    then
                        if LLVM.CountBasicBlocks(function') <> 0u 
                            then failwithf "redefinition of function %s" name
                        else if LLVM.CountParams(function') <> argumentCount 
                            then failwithf "redefinition of function %s with a different # of args" name
                    else
                        let arguments =
                            let n = Math.Max(int argumentCount, 1)
                            Array.create n (LLVM.DoubleType())
                        function' <-
                            let functionType = LLVM.FunctionType(LLVM.DoubleType(), arguments, false)
                            LLVM.AddFunction(module', name, functionType)
                        LLVM.SetLinkage(function', LLVMLinkage.LLVMExternalLinkage)
                Seq.iteri (fun i argumentName ->
                    let param = LLVM.GetParam(function', uint32 i)
                    LLVM.SetValueName(param, argumentName)
                    namedValues.Add(argumentName, param)
                ) arguments
                function'
              ) 
            | Function (proto, body) ->
                resultOf {
                    let! function' = codegenDecl (Prototype proto)
                    LLVM.PositionBuilderAtEnd(builder, LLVM.AppendBasicBlock(function', "entry"))
                    let! body =
                        try
                          codegenExpr  body
                        with _ ->
                          LLVM.DeleteFunction(function')
                          reraise ()
                    LLVM.BuildRet(builder, body) |> ignore
                    LLVM.VerifyFunction(function', LLVMVerifierFailureAction.LLVMPrintMessageAction) |> ignore
                    return function'
                }
        { new ICodeGenerator with
            member __.Run decl =
                codegenDecl decl |> Result.iter (fun exe -> LLVM.DumpValue(exe)) 
        }


[<RequireQualifiedAccess>]
module Driver =
    open Codegen
    open Parser

    exception DriverException of string

    let [<Literal>] Preamble = "Kaleidoscope in F# and LLVM\n"

    [<Sealed>]
    type Driver internal(module', engine, functionPassManager) =
        interface IDisposable with
            member __.Dispose () =
                LLVM.DumpModule(module')
                LLVM.DisposeModule(module')
                LLVM.DisposeExecutionEngine(!engine)
                LLVM.DisposePassManager functionPassManager
        member __.Run (parser: IParser) (codegen: ICodeGenerator) =
            let readLines () =
                let rec readLinesIntoSeq lines =
                    let nextLine = Console.ReadLine() 
                    if String.IsNullOrEmpty(nextLine)
                        then lines
                        else
                            do printf "-\t"
                            seq {
                                yield! lines
                                yield nextLine
                            } |> readLinesIntoSeq
                Seq.empty
                |> readLinesIntoSeq
                |> String.concat "\n"
            do printfn "%s" Preamble
            let prompt = "ready>\t"
            let rec runRepl () =
                resultOf {
                    printf "%s" prompt
                    let lines = readLines ()
                    if String.IsNullOrWhiteSpace(lines)
                        then do! runRepl ()
                        else
                            let! t = parser.RunOnString lines
                            do codegen.Run t
                            // do! codegen.Run t
                            return! runRepl ()
                }
            runRepl ()

    let Create module' engine =
        let init () =
            LLVM.LinkInMCJIT()
            LLVM.InitializeX86TargetInfo()
            LLVM.InitializeX86Target()
            LLVM.InitializeX86TargetMC()
            LLVM.InitializeX86AsmParser()
            LLVM.InitializeX86AsmPrinter()
            let mutable message = ""
            if 1 = (LLVM.CreateExecutionEngineForModule(engine, module', &message)).Value
                then Result.Error (DriverException message)
                else
                    let functionPassManager =  LLVM.CreateFunctionPassManagerForModule(module')


                    // Provide basic AliasAnalysis support for GVN.
                    LLVM.AddBasicAliasAnalysisPass(functionPassManager)

                    // Promote allocas to registers.
                    LLVM.AddPromoteMemoryToRegisterPass(functionPassManager)

                    // Do simple "peephole" optimizations and bit-twiddling optzns.
                    LLVM.AddInstructionCombiningPass(functionPassManager)

                    // Reassociate expressions.
                    LLVM.AddReassociatePass(functionPassManager)

                    // Eliminate Common SubExpressions.
                    LLVM.AddGVNPass(functionPassManager)

                    // Simplify the control flow graph (deleting unreachable blocks, etc).
                    LLVM.AddCFGSimplificationPass(functionPassManager)

                    ignore <| LLVM.InitializeFunctionPassManager(functionPassManager)

                    Result.Ok functionPassManager
        match init () with
        | Result.Ok functionPassManager -> new Driver(module', engine, functionPassManager) |> Result.Ok
        | Result.Error error -> Result.Error error


module Say =
    let hello name =
        printfn "Hello %s" name
