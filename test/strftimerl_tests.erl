-module(strftimerl_tests).

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
  ?assertEqual(": 7:", strftimerl:format(":%k:", Datetime)).

format_k17_test() ->
  Datetime = {{2002,12,20},{17,46,0}},
  ?assertEqual(":17:", strftimerl:format(":%k:", Datetime)).

format_l_test() ->
  Datetime = {{2002,12,20},{17,46,0}},
  ?assertEqual(": 5:", strftimerl:format(":%l:", Datetime)).

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
