/*****************************************************************************
  This experiment attempts to use the Clang C++ HeaderSearch API, not yet
  using Clang arguments.  It is not working yet.

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
    this.  This is my first ("simple") experiment.
  - The Clang C++ API changes across different versions.  This program was
    developed using LLVM 15.0.7 and a number of interfaces do not match the
    online reference.
*****************************************************************************/

#include <memory>

#include "clang/Basic/TargetInfo.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/TextDiagnosticPrinter.h"
#include "clang/Lex/HeaderSearch.h"
#include "clang/Lex/HeaderSearchOptions.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Lex/PreprocessorOptions.h"

#include "llvm/ADT/iterator_range.h"
#include "llvm/Support/Host.h"

int main() {
    // Target triple
    std::string triple_target = llvm::sys::getDefaultTargetTriple();
    llvm::outs() << "Target triple: " << triple_target << "\n";

    // Diagnostics
    llvm::IntrusiveRefCntPtr<clang::DiagnosticOptions> diag_opts =
        new clang::DiagnosticOptions();
    clang::TextDiagnosticPrinter diag_printer(llvm::errs(), &*diag_opts);

    // Compiler instance (helper class to manage Clang)
    std::unique_ptr<clang::CompilerInstance> ci =
        std::make_unique<clang::CompilerInstance>();
    ci->createDiagnostics(&diag_printer, false);
    ci->createFileManager();
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
