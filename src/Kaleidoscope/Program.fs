// Learn more about F# at http://fsharp.org

open System
open LLVMSharp
open Kaleidoscope


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
      let functionPassManager = LLVM.CreateFunctionPassManagerForModule(module')
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
      (* Run the main "interpreter loop" now. *)
      Driver.run(Console.In)
      LLVM.DumpModule(module')
      LLVM.DisposeModule(module')
      LLVM.DisposeExecutionEngine(!engine)
      LLVM.DisposePassManager(functionPassManager)
      0
