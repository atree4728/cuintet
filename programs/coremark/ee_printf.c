// CoreMark reports everything -- parameters, CRCs and the score -- through
// ee_printf.  cuintet has no I/O, so the port drops it all: the run is judged by
// crt0.S reaching its ECALL, which happens only once main() has returned.

#include "coremark.h"

int
ee_printf(const char *fmt, ...)
{
    (void)fmt;
    return 0;
}
