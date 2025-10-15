# Development Environment Setup

This guide covers setting up a development environment for `hs-bindgen` on
Linux, NixOS, and macOS.

## General Prerequisites

All platforms require (last updated October 9, 2025):
- GHC 9.4.8 or greater (or compatible version)
- Cabal (latest version)
- LLVM/Clang (version 14 - 21)

## Linux (Ubuntu)

### Required Packages

Install LLVM and Clang (package names may vary by distribution):

```bash
# Ubuntu
sudo apt-get install llvm-16 clang-16

# Other distributions
# Use your package manager to install LLVM and Clang version
```

### Environment Variables

Make sure the following environment variables are correctly set after
installing LLVM and Clang, otherwise set them as follows:

```bash
# Point to your LLVM installation
export LLVM_PATH=/usr/lib/llvm-16  # Adjust version as needed
export LLVM_CONFIG=$LLVM_PATH/bin/llvm-config
export LIBCLANG_PATH=$LLVM_PATH/lib/
```

## NixOS

For NixOS users, a flake is available that provides a complete development
environment:

```bash
# If you have the project checked out:
nix develop
# Or directly use the repository:
nix develop git+https://github.com/well-typed/hs-bindgen/
```

## macOS

macOS setup is similar to Linux but with platform-specific considerations.

### Prerequisites

Install LLVM and Clang:
```bash
brew install llvm@16
```

### Environment Setup

1. **Set LLVM paths**:
   ```bash
   export LLVM_PATH=/opt/homebrew/opt/llvm@16  # Adjust for your installation
   export LLVM_CONFIG=$LLVM_PATH/bin/llvm-config
   export LIBCLANG_PATH=$LLVM_PATH/lib/
   ```

2. **SDK configuration**:
   ```bash
   export SDKROOT=$(xcrun --show-sdk-path --sdk macosx)
   ```

3. **Library paths**:
   ```bash
   export DYLD_LIBRARY_PATH=/path/to/your/c/libs:$DYLD_LIBRARY_PATH
   ```

## Windows

### Prerequisites

On Windows, GHC installation via GHCup includes a MinGW environment with LLVM
and Clang, so no separate LLVM installation is needed.

### Environment Setup

1. Set LLVM paths (PowerShell):
   ```powershell
   $env:LLVM_PATH = "C:\ghcup\ghc\9.12.2\mingw"
   $env:LLVM_CONFIG = "$env:LLVM_PATH\bin\llvm-config.exe"
   $env:LIBCLANG_PATH = "$env:LLVM_PATH\lib"
   ```

2. Library paths:

   Windows uses `PATH` for finding DLLs:
   ```powershell
   $env:PATH = "C:\path\to\your\c\libs;" + $env:PATH
   ```

## Verifying Your Setup

Test your environment:

```bash
# Clone the repository
git clone https://github.com/well-typed/hs-bindgen.git
cd hs-bindgen

# Build the project
cabal build all
```

If successful, your environment is properly configured
