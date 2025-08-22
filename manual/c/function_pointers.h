/*
 * Function pointers
 */

extern int square(int);

extern int plus(int, int);

extern int apply1 (int (*f)(int), int x);

extern int apply2 (int (*f)(int, int), int x, int y);

/*
 * TODOs
 */

/*
TODO: the foreign imports of apply1 and apply2 use Ptr instead of FunPtr
because of the typedefs. We should figure out if hs-bindgen can look through
typedefs when deciding whether to use Ptr or FunPtr in foreign imports.

=== Example

> typedef int int2int(int);
> extern int apply (int2int * f, int x);
The foreign import for apply uses a Ptr where it should be using a FunPtr (right?)
> newtype Int2int = Int2int
>   { un_Int2int :: FC.CInt -> IO FC.CInt
>   }
>
> foreign import ccall safe "hs_bindgen_e89baabc0ea659ed" apply
>   :: F.Ptr Int2int
>   -> FC.CInt
>   -> IO FC.CInt
*/

/*
TODO: if a typedef is used as the type of a function, hs-bindgen will panic.
It should be perfectly valid as long as the typedef is a function type, but
can hs-bindgen look through such typedefs to detect this?

=== Example

> typedef int baloo(int);
> extern baloo balooFun;
*/

/*
TODO: in most contexts, a function (type) is implicitly converted to a
function pointer (type). For example, a function can either be declared to
take a function pointer argument or just a function argument, though they are
equivalent because of the implicit conversion. hs-bindgen handles a similar
implicit conversion for arrays, but not yet for functions.

<https://en.cppreference.com/w/c/language/conversion.html#Function_to_pointer_conversion>

=== Example

> extern int apply_pointer (int (*f)(int), int x);
> extern int apply_nopointer (int f(int), int x);

are equivalent but the foreign imports are different, and the foreign import
for the latter is even more wrong because it is missing parentheses.

> foreign import ccall safe "hs_bindgen_5553e6f9246adc97" apply_pointer
>   :: F.FunPtr (FC.CInt -> IO FC.CInt)
>   -> FC.CInt
>   -> IO FC.CInt
>
> foreign import ccall safe "hs_bindgen_d00aec9ae0c7c9a1" apply_nopointer
>   :: FC.CInt -> IO FC.CInt
>   -> FC.CInt
>   -> IO FC.CInt
*/
