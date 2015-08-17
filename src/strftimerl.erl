%% =============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2015 AWeber Communications
%% @end
%% =============================================================================
-module(strftimerl).
-author("gavinr").

%% API
-export([format/2]).

-define(CONVERSION_SPECIFICATIONS, ["%C", "%d", "%D", "%F", "%g", "%G", "%H",
                                    "%I", "%j", "%k", "%l", "%m", "%M", "%n",
                                    "%p", "%P", "%r", "%R", "%s", "%S", "%t",
                                    "%T", "%u", "%V", "%w", "%y", "%Y"]).

-spec format(Value :: string(), Datetime :: calendar:datetime()) -> string().
%% @spec format(Value, Datetime) -> string()
%% where
%%    Value = string()
%%    Datetime = datetime()
%% @end
%% @doc Return a interpolated version of the path string. Supported conversion specifictions
%%      include:
%%        - ``%C'' The century number (year/100) as a 2-digit integer.
%%        - ``%d'' The day of the month as a decimal number (range 01 to 31).
%%        - ``%D'' Equivalent to ``%m/%d/%y''.
%%        - ``%D'' Equivalent to ``%m/%d/%y''.
%%        - ``%F'' Equivalent to ``%Y-%m-%d''.
%%        - ``%G'' The ISO 8601 week-based year with century as a decimal number.
%%        - ``%g'' Like ``%G'', but without century, that is, with a 2-digit year (00-99).
%%        - ``%H'' The hour as a decimal number using a 24-hour clock (range 00 to 23).
%%        - ``%I'' The hour as a decimal number using a 12-hour clock (range 01 to 12).
%% @end
format(Value, Datetime) ->
  format(?CONVERSION_SPECIFICATIONS, Datetime, Value).

-spec format(Specifications :: [string()], Datetime :: calendar:datetime(), Value :: string()) -> string().
%% @private
%% @spec format(Specifications, Datetime, Value) -> string()
%% where
%%    Specifications = [string()]
%%    Value = string()
%%    Datetime = datetime()
%% @doc Return a interpolated version of the path string.
%% @end
%%
format([], _, Value) -> Value;

format([H|T], Datetime, Value) when H == "%C" ->
  {{V, _, _}, _} = Datetime,
  format(T, Datetime, replace(H, Value, lists:sublist(as_string(V), 1, 2)));

format([H|T], Datetime, Value) when H == "%d" ->
  {{_, _, D},_} = Datetime,
  V = lists:flatten(io_lib:format("~2..0B", [D])),
  format(T, Datetime, replace(H, Value, as_string(V)));

format([H|T], Datetime, Value) when H == "%D" ->
  {{Y, M, D},_} = Datetime,
  V = lists:flatten(io_lib:format("~2..0B/~2..0B/~4..0B", [M, D, Y])),
  format(T, Datetime, replace(H, Value, as_string(V)));

format([H|T], Datetime, Value) when H == "%F" ->
  {{Y, M, D},_} = Datetime,
  V = lists:flatten(io_lib:format("~4..0B-~2..0B-~2..0B", [Y, M, D])),
  format(T, Datetime, replace(H, Value, as_string(V)));

format([H|T], Datetime, Value) when H == "%g" ->
  {Date,_} = Datetime,
  {Y, _} = calendar:iso_week_number(Date),
  format(T, Datetime, replace(H, Value, lists:nthtail(2, as_string(Y))));

format([H|T], Datetime, Value) when H == "%G" ->
  {Date,_} = Datetime,
  {Y, _} = calendar:iso_week_number(Date),
  format(T, Datetime, replace(H, Value, as_string(Y)));

format([H|T], Datetime, Value) when H == "%H"; H == "%k" ->
  {_,{V,_,_}} = Datetime,
  Hour = lists:flatten(io_lib:format("~2..0B", [V])),
  format(T, Datetime, replace(H, Value, Hour));

format([H|T], Datetime, Value) when H == "%I"; H == "%l" ->
  {_,{V,_,_}} = Datetime,
  Hour = case V > 12 of
    true  -> lists:flatten(io_lib:format("~2..0B", [V - 12]));
    false -> lists:flatten(io_lib:format("~2..0B", [V]))
  end,
  format(T, Datetime, replace(H, Value, Hour));

format([H|T], Datetime, Value) when H == "%j" ->
  {Date,_} = Datetime,
  {_, W} = calendar:iso_week_number(Date),
  V = ((W-1) * 7) + (calendar:day_of_the_week(Date) - 1),
  Hour = lists:flatten(io_lib:format("~3..0B", [V])),
  format(T, Datetime, replace(H, Value, Hour));

format([H|T], Datetime, Value) when H == "%m" ->
  {{_, M, _},_} = Datetime,
  V = lists:flatten(io_lib:format("~2..0B", [M])),
  format(T, Datetime, replace(H, Value, as_string(V)));

format([H|T], Datetime, Value) when H == "%M" ->
  {_,{_, M, _}} = Datetime,
  V = lists:flatten(io_lib:format("~2..0B", [M])),
  format(T, Datetime, replace(H, Value, as_string(V)));

format([H|T], Datetime, Value) when H == "%n" ->
  format(T, Datetime, replace(H, Value, "\n"));

format([H|T], Datetime, Value) when H == "%p" ->
  {_,{V,_,_}} = Datetime,
  Hour = case V > 12 of
    true  -> "PM";
    false -> "AM"
  end,
  format(T, Datetime, replace(H, Value, Hour));

format([H|T], Datetime, Value) when H == "%P" ->
  {_,{V,_,_}} = Datetime,
  Hour = case V > 12 of
    true  -> "pm";
    false -> "am"
  end,
  format(T, Datetime, replace(H, Value, Hour));

format([H|T], Datetime, Value) when H == "%r" ->
  {_,{Hr, M, S}} = Datetime,
  {Hour, I} = case Hr > 12 of
    true  -> {Hr - 12, "PM"};
    false -> {Hr, "AM"}
  end,
  V = lists:flatten(io_lib:format("~2..0B:~2..0B:~2..0B ~s", [Hour, M, S, I])),
  format(T, Datetime, replace(H, Value, as_string(V)));

format([H|T], Datetime, Value) when H == "%R" ->
  {_,{Hr, M, _}} = Datetime,
  V = lists:flatten(io_lib:format("~2..0B:~2..0B", [Hr, M])),
  format(T, Datetime, replace(H, Value, as_string(V)));

format([H|T], Datetime, Value) when H == "%s" ->
  V = calendar:datetime_to_gregorian_seconds(Datetime) - 62167219200,
  format(T, Datetime, replace(H, Value, as_string(V)));

format([H|T], Datetime, Value) when H == "%S" ->
  {_,{_, _, S}} = Datetime,
  V = lists:flatten(io_lib:format("~2..0B", [S])),
  format(T, Datetime, replace(H, Value, as_string(V)));

format([H|T], Datetime, Value) when H == "%t" ->
  format(T, Datetime, replace(H, Value, "\t"));

format([H|T], Datetime, Value) when H == "%T" ->
  {_,{Hr, M, S}} = Datetime,
  V = lists:flatten(io_lib:format("~2..0B:~2..0B:~2..0B", [Hr, M, S])),
  format(T, Datetime, replace(H, Value, as_string(V)));

format([H|T], Datetime, Value) when H == "%u" ->
  {Date,_} = Datetime,
  V = calendar:day_of_the_week(Date),
  format(T, Datetime, replace(H, Value, as_string(V)));

format([H|T], Datetime, Value) when H == "%V" ->
  {Date,_} = Datetime,
  {_, W} = calendar:iso_week_number(Date),
  V = lists:flatten(io_lib:format("~2..0B", [W])),
  format(T, Datetime, replace(H, Value, V));

format([H|T], Datetime, Value) when H == "%w" ->
  {Date,_} = Datetime,
  V = case calendar:day_of_the_week(Date) of
    7 -> 0;
    O -> O
  end,
  format(T, Datetime, replace(H, Value, as_string(V)));

format([H|T], Datetime, Value) when H == "%y" ->
  {{Y, _, _},_} = Datetime,
  format(T, Datetime, replace(H, Value, as_string(lists:nthtail(2, as_string(Y)))));

format([H|T], Datetime, Value) when H == "%Y" ->
  {{V, _, _},_} = Datetime,
  format(T, Datetime, replace(H, Value, as_string(V)));

format([H|T], Datetime, Value) ->
  error_logger:warning_msg("Unsupported specifier ~s", [H]),
  format(T, Datetime, Value).


-spec as_string(Needle :: atom() | integer() | binary() | list()) -> string().
%% @private
%% @spec as_string(Value) ->string()
%% where Value = list()|integer()
%% @doc Return the value as a list
%% @end
%%
as_string([]) -> undefined;
as_string(Value) when is_atom(Value) =:= true -> atom_to_list(Value);
as_string(Value) when is_binary(Value) =:= true -> binary_to_list(Value);
as_string(Value) when is_integer(Value) =:= true -> integer_to_list(Value);
as_string(Value) when is_list(Value) =:= true -> Value;
as_string(Value) -> Value.


-spec replace(Needle :: string(), Haystack :: string(), Value :: string()) -> string().
%% @private
%% @doc Replace Needle with Value in Haystack
%% @spec replace(Needle, Haystack, Value) -> string()
%% where
%%    Needle = string()
%%    Haystack = string()
%%    Value = string()
%% @end
%%
replace(Needle, Haystack, Value) ->
  case string:str(Haystack, Needle) of
    0 -> Haystack;
    P ->
      New = string:left(Haystack, P - 1) ++ Value ++ string:right(Haystack, length(Haystack) - P - 1),
      replace(Needle, New, Value)
  end.

%% Unit Tests

-ifdef(DEV_ONLY).

-include_lib("eunit/include/eunit.hrl").

as_string_binary_test() ->
  ?assertEqual("foo", strftimerl:as_string(<<"foo">>)).

as_string_list_test() ->
  ?assertEqual("bar", strftimerl:as_string("bar")).

as_string_integer_test() ->
  ?assertEqual("42", strftimerl:as_string(42)).

format_C_test() ->
  Datetime = {{2002,12,20},{7,46,0}},
  ?assertEqual(":20:", strftimerl:format(":%C:", Datetime)).

format_d_test() ->
  Datetime = {{2002,12,20},{7,46,0}},
  ?assertEqual(":20:", strftimerl:format(":%d:", Datetime)).

format_d_single_digit_test() ->
  Datetime = {{2002,12,2},{7,46,0}},
  ?assertEqual(":02:", strftimerl:format(":%d:", Datetime)).

format_D_test() ->
  Datetime = {{2002,12,20},{7,46,0}},
  ?assertEqual(":12/20/2002:", strftimerl:format(":%D:", Datetime)).

format_F_test() ->
  Datetime = {{2002,12,20},{7,46,0}},
  ?assertEqual(":2002-12-20:", strftimerl:format(":%F:", Datetime)).

format_g_test() ->
  Datetime = {{2002,12,20},{7,46,0}},
  ?assertEqual(":02:", strftimerl:format(":%g:", Datetime)).

format_G_test() ->
  Datetime = {{2002,12,20},{7,46,0}},
  ?assertEqual(":2002:", strftimerl:format(":%G:", Datetime)).

format_H_test() ->
  Datetime = {{2002,12,20},{7,46,0}},
  ?assertEqual(":07:", strftimerl:format(":%H:", Datetime)).

format_H17_test() ->
  Datetime = {{2002,12,20},{17,46,0}},
  ?assertEqual(":17:", strftimerl:format(":%H:", Datetime)).

format_I_test() ->
  Datetime = {{2002,12,20},{17,46,0}},
  ?assertEqual(":05:", strftimerl:format(":%I:", Datetime)).

format_j_test() ->
  Datetime = {{2002,12,20},{17,46,0}},
  ?assertEqual(":354:", strftimerl:format(":%j:", Datetime)).

format_j2_test() ->
  Datetime = {{2002,9,6},{17,46,0}},
  ?assertEqual(":249:", strftimerl:format(":%j:", Datetime)).

format_k_test() ->
  Datetime = {{2002,12,20},{7,46,0}},
  ?assertEqual(":07:", strftimerl:format(":%k:", Datetime)).

format_k17_test() ->
  Datetime = {{2002,12,20},{17,46,0}},
  ?assertEqual(":17:", strftimerl:format(":%k:", Datetime)).

format_l_test() ->
  Datetime = {{2002,12,20},{17,46,0}},
  ?assertEqual(":05:", strftimerl:format(":%l:", Datetime)).

format_m_test() ->
  Datetime = {{2002,12,20},{17,46,0}},
  ?assertEqual(":12:", strftimerl:format(":%m:", Datetime)).

format_M_test() ->
  Datetime = {{2002,12,20},{17,46,0}},
  ?assertEqual(":46:", strftimerl:format(":%M:", Datetime)).

format_M_single_digit_test() ->
  Datetime = {{2002,12,20},{17,4,0}},
  ?assertEqual(":04:", strftimerl:format(":%M:", Datetime)).

format_n_test() ->
  Datetime = {{2002,12,20},{17,46,0}},
  ?assertEqual(":\n:", strftimerl:format(":%n:", Datetime)).

format_p_am_test() ->
  Datetime = {{2002,12,20},{7,46,0}},
  ?assertEqual(":AM:", strftimerl:format(":%p:", Datetime)).

format_p_pm_test() ->
  Datetime = {{2002,12,20},{17,46,0}},
  ?assertEqual(":PM:", strftimerl:format(":%p:", Datetime)).

format_P_am_test() ->
  Datetime = {{2002,12,20},{7,46,0}},
  ?assertEqual(":am:", strftimerl:format(":%P:", Datetime)).

format_P_pm_test() ->
  Datetime = {{2002,12,20},{17,46,0}},
  ?assertEqual(":pm:", strftimerl:format(":%P:", Datetime)).

format_r_am_test() ->
  Datetime = {{2002,12,20},{17,46,10}},
  ?assertEqual(":05:46:10 PM:", strftimerl:format(":%r:", Datetime)).

format_r_pm_test() ->
  Datetime = {{2002,12,20},{7,46,10}},
  ?assertEqual(":07:46:10 AM:", strftimerl:format(":%r:", Datetime)).

format_R_am_test() ->
  Datetime = {{2002,12,20},{7,46,10}},
  ?assertEqual(":07:46:", strftimerl:format(":%R:", Datetime)).

format_R_pm_test() ->
  Datetime = {{2002,12,20},{17,46,10}},
  ?assertEqual(":17:46:", strftimerl:format(":%R:", Datetime)).

format_s_test() ->
  Datetime = {{2002,12,20},{7,46,10}},
  ?assertEqual(":1040370370:", strftimerl:format(":%s:", Datetime)).

format_S_test() ->
  Datetime = {{2002,12,20},{17,46,10}},
  ?assertEqual(":10:", strftimerl:format(":%S:", Datetime)).

format_S_single_digit_test() ->
  Datetime = {{2002,12,20},{17,46,5}},
  ?assertEqual(":05:", strftimerl:format(":%S:", Datetime)).

format_t_test() ->
  Datetime = {{2002,12,20},{17,46,0}},
  ?assertEqual(":\t:", strftimerl:format(":%t:", Datetime)).

format_T_test() ->
  Datetime = {{2002,12,20},{17,46,0}},
  ?assertEqual(":17:46:00:", strftimerl:format(":%T:", Datetime)).

format_T_single_digit_test() ->
  Datetime = {{2002,12,20},{7,46,0}},
  ?assertEqual(":07:46:00:", strftimerl:format(":%T:", Datetime)).

format_u_test() ->
  Datetime = {{2002,12,20},{17,46,0}},
  ?assertEqual(":5:", strftimerl:format(":%u:", Datetime)).

format_V_test() ->
  Datetime = {{2002,12,15},{17,46,0}},
  ?assertEqual(":50:", strftimerl:format(":%V:", Datetime)).

format_w_test() ->
  Datetime = {{2002,12,15},{17,46,0}},
  ?assertEqual(":0:", strftimerl:format(":%w:", Datetime)).

format_y_test() ->
  Datetime = {{2002,12,15},{17,46,0}},
  ?assertEqual(":02:", strftimerl:format(":%y:", Datetime)).

format_Y_test() ->
  Datetime = {{2002,12,15},{17,46,0}},
  ?assertEqual(":2002:", strftimerl:format(":%Y:", Datetime)).

replace_test() ->
  ?assertEqual("2002 foo 2002bar2002.", strftimerl:replace("%y", "%y foo %ybar%y.", "2002")).

-endif.
