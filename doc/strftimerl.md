

# Module strftimerl #
* [Function Index](#index)
* [Function Details](#functions)

Copyright (c) 2015 AWeber Communications

__Authors:__ Gavin M. Roy ([`gavinr@aweber.com`](mailto:gavinr@aweber.com)).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#format-2">format/2</a></td><td>Return a interpolated version of the path string.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="format-2"></a>

### format/2 ###

<pre><code>
format(Value, Datetime) -&gt; string()
</code></pre>

<ul class="definitions"><li><code>Value = string()</code></li><li><code>Datetime = <a href="#type-datetime">datetime()</a></code></li></ul>

Return a interpolated version of the path string. Supported conversion specifictions
include:
- `%C` The century number (year/100) as a 2-digit integer.
- `%d` The day of the month as a decimal number (range 01 to 31).
- `%D` Equivalent to `%m/%d/%y`.
- `%D` Equivalent to `%m/%d/%y`.
- `%F` Equivalent to `%Y-%m-%d`.
- `%G` The ISO 8601 week-based year with century as a decimal number.
- `%g` Like `%G`, but without century, that is, with a 2-digit year (00-99).
- `%H` The hour as a decimal number using a 24-hour clock (range 00 to 23).
- `%I` The hour as a decimal number using a 12-hour clock (range 01 to 12).

