/*****************************************************************************
  This experiment attempts to use the Clang C++ HeaderSearch API, using the
  specified Clang arguments.  It is not working yet.

  This program prints out the include search path directories.  None are
  printed, however, even when one is specified on the command line.  Once this
  is fixed, we need to confirm that the behavior matches the libclang behavior
  as used by hs-bindgen.

  - The same default include directories are used.
  - Environment variables such as C_INCLUDE_PATH are processed.

  Once the include search path is working, the goal is to resolve headers.
  Note that Preprocessor::LookupFile should probably be used instead of using
  HeaderSearch::LookupFile directly.

  Notes:

  - The Clang API is pretty large, and I am not sure of the best way to do
    this.  This experiment uses a driver because I think that it is the
    standard way to process Clang command-line arguments.
  - The Clang C++ API changes across different versions.  This program was
    developed using LLVM 15.0.7 and a number of interfaces do not match the
    online reference.
  - A triple needs to be specified to instantiate a driver, before arguments
    are parsed.  Something probably needs to change if we want to use a target
    specified via arguments.
  - Exactly one header file needs to be specified as an argument.  The
    including file must be specified to do header resolution when there are
    relative paths involved.
*****************************************************************************/

#include <memory>
#include <vector>

#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/DiagnosticOptions.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/Driver/Compilation.h"
#include "clang/Driver/Driver.h"
#include "clang/Driver/Job.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/CompilerInvocation.h"
#include "clang/Frontend/TextDiagnosticPrinter.h"
#include "clang/Lex/HeaderSearch.h"
#include "clang/Lex/HeaderSearchOptions.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Lex/PreprocessorOptions.h"

#include "llvm/Support/Host.h"
#include "llvm/Support/VirtualFileSystem.h"

int main(int argc, char *argv[]) {
    // Clang command-line arguments (strings)
    std::vector<const char *> args = {"clang", "-E"};
    args.insert(args.end(), argv + 1, argv + argc);

    // Target triple (default, unrelated to arguments)
    std::string triple_target = llvm::sys::getDefaultTargetTriple();
    llvm::outs() << "Target triple: " << triple_target << "\n";

    // Diagnostics
    llvm::IntrusiveRefCntPtr<clang::DiagnosticIDs> diag_ids =
        new clang::DiagnosticIDs();
    llvm::IntrusiveRefCntPtr<clang::DiagnosticOptions> diag_opts =
        new clang::DiagnosticOptions();
    clang::TextDiagnosticPrinter diag_printer(llvm::errs(), &*diag_opts);
    llvm::IntrusiveRefCntPtr<clang::DiagnosticsEngine> diag_engine =
        new clang::DiagnosticsEngine(
            diag_ids, diag_opts, &diag_printer, false);

    // Filesystem
    llvm::IntrusiveRefCntPtr<llvm::vfs::FileSystem> fs =
        llvm::vfs::getRealFileSystem();

    // Driver, compilation, and job
    //
    // The driver processes the command-line arguments.  A compilation is
    // built from the driver.  A compilation may have any number of jobs,
    // typically corresponding to input arguments.  We can get the LLVM
    // arguments from a job.
    clang::driver::Driver driver(
        args[0], triple_target, *diag_engine, "exdriver", fs);
    driver.setCheckInputsExist(false);
    std::unique_ptr<clang::driver::Compilation> driver_comp(
        driver.BuildCompilation(args));
    if (!driver_comp || driver_comp->containsError()) {
        llvm::errs() << "error: failed to build compilation\n";
        return 1;
    }
    const clang::driver::JobList &driver_jobs = driver_comp->getJobs();
    if (driver_jobs.size() != 1) {
        llvm::errs() << "error: only one job supported\n";
        return 1;
    }
    const llvm::opt::ArgStringList &llvm_args =
        driver_jobs.begin()->getArguments();

    // Compiler invocation (created using the LLVM arguments)
    std::unique_ptr<clang::CompilerInvocation> invoc =
        std::make_unique<clang::CompilerInvocation>();
    clang::CompilerInvocation::CreateFromArgs(
        *invoc, llvm_args, *diag_engine);

    // Compiler instance (helper class to manage Clang)
    std::unique_ptr<clang::CompilerInstance> ci =
        std::make_unique<clang::CompilerInstance>();
    ci->setInvocation(std::move(invoc));
    ci->createDiagnostics(&diag_printer, false);
    ci->createFileManager(fs);
    ci->createSourceManager(ci->getFileManager());

    // Target (must be set to create a preprocessor)
    std::shared_ptr<clang::TargetOptions> target_opts =
        std::make_shared<clang::TargetOptions>();
    target_opts->Triple = triple_target;
    clang::TargetInfo *target_info = clang::TargetInfo::CreateTargetInfo(
        ci->getDiagnostics(), target_opts);
    ci->setTarget(target_info);

    // Preprocessor
    ci->createPreprocessor(clang::TU_Complete);
    clang::Preprocessor& pp = ci->getPreprocessor();
    clang::PreprocessorOptions& pp_opts = ci->getPreprocessorOpts();
    pp_opts.UsePredefines = true; // not required because default?
    clang::InitializePreprocessor(
        pp, pp_opts, ci->getPCHContainerReader(), ci->getFrontendOpts());

    // HeaderSearch
    clang::HeaderSearch& hs = pp.getHeaderSearchInfo();
    clang::HeaderSearchOptions hs_opts;
    hs_opts.UseStandardSystemIncludes = 1; // not required because default?
    clang::ApplyHeaderSearchOptions(
        hs, hs_opts, ci->getLangOpts(), target_info->getTriple());

    llvm::outs() << "Search path directories:\n";
    for (const auto &d :
            llvm::make_range(hs.search_dir_begin(), hs.search_dir_end())) {
        llvm::outs() << "  " << d.getDir()->getName().str() << "\n";
    }

    llvm::outs() << "Angled search path directories:\n";
    for (const auto &d :
            llvm::make_range(hs.angled_dir_begin(), hs.angled_dir_end())) {
        llvm::outs() << "  " << d.getDir()->getName().str() << "\n";
    }

    llvm::outs() << "Quoted search path directories:\n";
    for (const auto &d :
            llvm::make_range(hs.quoted_dir_begin(), hs.quoted_dir_end())) {
        llvm::outs() << "  " << d.getDir()->getName().str() << "\n";
    }

    llvm::outs() << "System search path directories:\n";
    for (const auto &d :
            llvm::make_range(hs.system_dir_begin(), hs.system_dir_end())) {
        llvm::outs() << "  " << d.getDir()->getName().str() << "\n";
    }

    return 0;
}
