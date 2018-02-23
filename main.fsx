#!/usr/bin/env fsharpi
// References generated via `mono .paket/paket.exe generate-load-scripts`
#load ".paket/load/net471/main.group.fsx"
#r "src/Kaleidoscope/bin/Release/netcoreapp2.0/Kaleidoscope.dll"
open System
open LLVMSharp
open Kaleidoscope

let [<Literal>] JitName = "my cool jit"
let context = LLVM.GetGlobalContext()
let module' = LLVM.ModuleCreateWithNameInContext(JitName, context)

let builder = LLVM.CreateBuilderInContext(context)
let engine = ref (LLVMExecutionEngineRef(IntPtr.Zero))

let driver = Driver.Create module' engine
do
    match driver with
    | Ok driver ->
        driver.Run
            Console.In
            (Parser.Create())
            (Codegen.Create module' builder context)
        driver.Dispose()
    | Error error -> failwith error
