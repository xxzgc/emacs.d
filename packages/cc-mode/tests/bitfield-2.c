struct
{
    signed EMACS_INT val : VALBITS;
    enum Lisp_Type type : GCTYPEBITS;
} s;
struct
{
    EMACS_UINT val : VALBITS,
        type : TYPEBITS;
} u;
