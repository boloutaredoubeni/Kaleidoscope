#!/usr/bin/env fsharpi
open System
// References generated via `mono .paket/paket.exe generate-load-scripts`
#load ".paket/load/netcoreapp2.0/LLVMSharp.fsx"
#r "src/Kaleidoscope/bin/Release/netcoreapp2.0/Kaleidoscope.dll"
do
    let libLlvmDllPath =
        let os =
            match Environment.OSVersion.Platform with
            | PlatformID.MacOSX -> "osx"
            | PlatformID.Unix -> "linux-x64"
            | _ -> "win-x64"
        __SOURCE_DIRECTORY__
        + "/packages/libLLVM/runtimes/"
        + os
        + "/native"
    printfn "%s" libLlvmDllPath
    Environment.SetEnvironmentVariable("Path",
        Environment.GetEnvironmentVariable("Path") + ";" + libLlvmDllPath)

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