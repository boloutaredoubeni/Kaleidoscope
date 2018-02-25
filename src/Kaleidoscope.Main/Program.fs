// Learn more about F# at http://fsharp.org
open System
open LLVMSharp
open Kaleidoscope

let [<Literal>] JitName = "my cool jit"


[<EntryPoint>]
let main argv =


    let context = LLVM.GetGlobalContext()
    let module' = LLVM.ModuleCreateWithNameInContext(JitName, context)

    let builder = LLVM.CreateBuilderInContext(context)
    let engine = ref (LLVMExecutionEngineRef(IntPtr.Zero))

    do 
        resultOf {
            let! driver = Driver.Create module' engine
            do! driver.Run (Parser.Create()) (Codegen.Create module' builder context)
        } |> Result.throw
    0 // return an integer exit code
