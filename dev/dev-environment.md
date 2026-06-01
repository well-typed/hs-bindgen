# Development Environment Setup

This guide covers setting up a development environment for `hs-bindgen` on
Linux, NixOS, and macOS.

## General Prerequisites

All platforms require (last updated October 9, 2025):

- GHC 9.4.8 or greater (or compatible version)
- Cabal (latest version)
- LLVM/Clang (version 14 - 22)

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

Make sure that the `llvm-config` for the version of LLVM/Clang that you want
to use is available.

```bash
llvm-config --version
```

If the command is not found or is for a different version, prepend the `bin`
for the LLVM/Clang version that you want to use to your `PATH`.

```
export PATH="/usr/lib/llvm-16/bin:${PATH}"  # Adjust version as needed
```

If `llvm-config` is not included in your installation, use the `LLVM_PATH`
environment variable instead.

```
export LLVM_PATH=/usr/lib/llvm-16  # Adjust version as needed
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

1. **Path configuration**:

   ```bash
   export PATH="/opt/homebrew/opt/llvm@16/bin:${PATH}"  # Adjust for your installation
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

1. Set LLVM path (PowerShell):

   ```powershell
   $env:PATH = "C:\ghcup\ghc\9.12.2\mingw\bin;" + $env:PATH
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
