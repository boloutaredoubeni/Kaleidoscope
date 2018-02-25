#!/usr/bin/env fsharpi
// References generated via `mono .paket/paket.exe generate-load-scripts`
#load ".paket/load/netcoreapp2.0/LLVMSharp.fsx"
#r "src/Kaleidoscope/bin/Release/netcoreapp2.0/Kaleidoscope.dll"
open System
open LLVMSharp
open Kaleidoscope

let [<Literal>] JitName = "my cool jit"
let context = LLVM.GetGlobalContext()
let module' = LLVM.ModuleCreateWithNameInContext(JitName, context)

let builder = LLVM.CreateBuilderInContext(context)
let engine = ref (LLVMExecutionEngineRef(IntPtr.Zero))

do 
    resultOf {
        let! driver = Driver.Create module' engine
        do! driver.Run (Parser.Create()) (Codegen.Create module' builder context)
    } |> Result.throw