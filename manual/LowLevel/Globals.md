# Global variables

## Example usage

Consider

```c
struct globalConfig {
  int numThreads;
  int numWorkers;
};

extern struct globalConfig globalConfig;

void printGlobalConfig();
```

This results in

```hs
foreign import capi safe "example.h &globalConfig"
  globalConfig :: Ptr GlobalConfig
```

along with the usual definitions for structs and functions:

```hs
data GlobalConfig = GlobalConfig {
      globalConfig_numThreads :: CInt
    , globalConfig_numWorkers :: CInt
    }
  deriving stock (Eq, Show)

instance Storable GlobalConfig where (..)

foreign import (..) printGlobalConfig :: IO ()
```

We can use these bindings as follows:

```hs
do config <- peek globalConfig
   print config      -- Print it Haskell side
   poke globalConfig $ config{globalConfig_numThreads = 3}
   printGlobalConfig -- Print it C side
```

## Non-extern globals

Some headers define globals without declaring them to be `extern`, for example

```c
int nonExternGlobalInt = 8;
```

The code `hs-bindgen` produces for this is as expected:

```hs
foreign import capi safe "&nonExternGlobalInt"
  nonExternGlobalInt :: F.Ptr FC.CInt
```

However, such headers need to be treated with caution: if they are included more
than once, the resulting binary will have duplicate symbols, resulting in linker
errors ("multiple definition of `nonExternGlobalInt'").

## Unsupported: thread-local variables

We currently do not generate bindings for global variables that are marked
`thread_local` ([#828](https://github.com/well-typed/hs-bindgen/issues/828)):

```c
thread_local extern int threadLocal;
```

Taking a pointer of such a variable may not be safe, and we should generate a
getter and setter instead. Such "global thread-local variables" should be
exceedingly rare.

## Invalid examples

Since `extern` merely declares the existence of a global variable, there must be
an actual definition of it in some C file. Consequently, anonymous declarations
inside `extern`s are unusable (as the corresponding definition of the global in
the C file would be unable to give it a value of the _same_ struct). Therefore

```c
extern struct {
  int numThreads;
  int numWorkers;
} globalConfig;
```

will result in a warning, and not produce any bindings.

# Constants

TODO