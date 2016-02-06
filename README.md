strftimerl
==========
Erlang implementation of strftime. This library implements most of the strftime format specifiers. It currently does not
support the any of the locale related text specifiers (day of week name, month name, etc), ``%U``, ``%W``, 
timezone specifiers, or the locale preferred date specifiers (``%x``, ``%X``).

[![Version](https://img.shields.io/hexpm/v/strftimerl.svg)](https://hex.pm/packages/strftimerl) [![Downloads](https://img.shields.io/hexpm/dt/strftimerl.svg)](https://hex.pm/packages/strftimerl) [![Build Status](https://travis-ci.org/gmr/strftimerl.svg?branch=master)](https://travis-ci.org/gmr/strftimerl) [![codecov.io](https://codecov.io/github/gmr/strftimerl/coverage.svg?branch=master)](https://codecov.io/github/gmr/strftimerl?branch=master)

### format/2 ###
```erlang
format(Value, Datetime) -> string()
```

<ul class="definitions"><li><code>Value = string()</code></li><li><code>Datetime = datetime()</a></code></li></ul>

Format a date and time. Supported conversion specifications include:

 Specifier | Meaning
-----------|---------
 ``%C``    | The century number (year/100) as a 2-digit integer.
 ``%d``    | The day of the month as a decimal number (range 01 to 31).
 ``%D``    | Equivalent to ``%m/%d/%y``.
 ``%F``    | Equivalent to ``%Y-%m-%d``.
 ``%G``    | The ISO 8601 week-based year with century as a decimal number.
 ``%g``    | Like ``%G``, but without century, that is, with a 2-digit year (00-99).
 ``%H``    | The hour as a decimal number using a 24-hour clock (range 00 to 23).
 ``%I``    | The hour as a decimal number using a 12-hour clock (range 01 to 12).
 ``%j``    | The day of the year as a decimal number (range 001 to 366).
 ``%k``    | The hour (24-hour clock) as a decimal number (range 0 to 23); single digits are preceded by a blank.  (See also ``%H``)
 ``%l``    | The hour (12-hour clock) as a decimal number (range 1 to 12); single digits are preceded by a blank.  (See also ``%I``)
 ``%m``    | The month as a decimal number (range 01 to 12).
 ``%M``    | The minute as a decimal number (range 00 to 59).
 ``%n``    | A newline character.
 ``%p``    | Either "AM" or "PM" according to the given time value. Noon is treated as "PM" and midnight as "AM".
 ``%P``    | Like ``%p`` but in lowercase: "am" or "pm".
 ``%r``    | The time in a.m. or p.m. notation. This is equivalent to ``%I:%M:%S %p``.
 ``%R``    | The time in 24-hour notation (``%H:%M``).  For a version including the seconds, see ``%T`` below.
 ``%s``    | The number of seconds since the Epoch, 1970-01-01 00:00:00 +0000 (UTC).
 ``%S``    | The second as a decimal number (range 00 to 60).  The range is up to 60 to allow for occasional leap seconds. 
 ``%t``    | A tab character.
 ``%T``    | The time in 24-hour notation (``%H:%M:%S``).
 ``%u``    | The day of the week as a decimal, range 1 to 7, Monday being 1.  See also ``%w``.
 ``%V``    | The ISO 8601 week number of the current year as a decimal number, range 01 to 53, where week 1 is the first week that has at least 4 days in the new year.
 ``%w``    | The day of the week as a decimal, range 0 to 6, Sunday being 0.  See also ``%u``.
 ``%y``    | The year as a decimal number without a century (range 00 to 99).
 ``%Y``    | The year as a decimal number including the century.

#### Examples ####

```erlang
Erlang/OTP 17 [erts-6.4] [source] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]

Eshell V6.4  (abort with ^G)
1> strftimerl:format("%Y-%m-%d %H:%M:%S", calendar:universal_time()).
"2015-08-17 22:02:26"
2> strftimerl:format("%s", calendar:universal_time()).
"1439848958"
3> strftimerl:format("%D", calendar:universal_time()).
"08/17/2015"
4> io:format("~s~n", [strftimerl:format("%Y-%m-%d%n%I:%M %P", calendar:universal_time())]).
2015-08-17
10:04 pm
ok    
```
