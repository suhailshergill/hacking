#include <time.h>
#include <stdio.h>
#include <string.h>

#include "timezone.h"

int isDst(int yr, int mon, int mday, int hr, int min, int sec)
{
    struct tm when;
    memset(&when, 0, sizeof(when));
    when.tm_sec = sec;
    when.tm_min = min;
    when.tm_hour = hr;
    when.tm_mday = mday;
    when.tm_mon = mon -1;
    when.tm_year = yr - 1900;
    when.tm_isdst = -1;
    time_t secs = mktime(&when);
    /* fprintf(stdout, "%ld %d %s", secs, when.tm_isdst, asctime(&when)); */
    return when.tm_isdst;
}

/* int main(int argc, char **argv) */
/* { */
/*   isDst(2007,3,26,8,30,0); */
/* } */
