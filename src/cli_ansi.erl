-module(cli_ansi).
-export([escape/1]).

escape(BitString) ->
    case io:columns(group_leader()) of
        {ok, _} -> escape(BitString, [], true);
        _Else   -> escape(BitString, [], false)
    end.
escape(<< $%, ${, Rest/binary >>, Acc, Emit) ->
    {Sequence, NewRest} = extract_sequence(Rest, []),
    escape(NewRest, [encode(Sequence, Emit)|Acc], Emit);
escape(<< X:1/binary, Rest/binary >>, Acc, Emit) ->
    escape(Rest, [X|Acc], Emit);
escape(<<"">>, Acc, _Emit) ->
    list_to_binary(lists:reverse(Acc)).

extract_sequence(<< $,, Rest/binary >>, Acc) ->
    extract_sequence(<<$}, $%, ${, Rest/binary>>, Acc);
extract_sequence(<< $\s, Rest/binary >>, Acc) ->
    extract_sequence(Rest, Acc);
extract_sequence(<< $}, Rest/binary >>, Acc) ->
    {list_to_binary(lists:reverse(Acc)), Rest};
extract_sequence(<< X:1/binary, Rest/binary>>, Acc) ->
    extract_sequence(Rest, [X|Acc]).   

encode(Sequence, _Emit=true) ->
    encode(Sequence);
encode(_Sequence, _Emit) ->
    <<"">>.
%% Resets all attributes
encode(<<"reset">>)                -> sequence(0);
%% Bright (increased intensity) or Bold
encode(<<"bright">>)               -> sequence(1);
%% Faint (decreased intensity), not widely supported
encode(<<"faint">>)                -> sequence(2);
%% Italic: on. Not widely supported. Sometimes treated as inverse.
encode(<<"italic">>)               -> sequence(3);
%% Underline: Single
encode(<<"underline">>)            -> sequence(4);
%% Blink: Slow. Less than 150 per minute
encode(<<"blink_slow">>)           -> sequence(5);
%% Blink: Rapid. MS-DOS ANSI.SYS; 150 per minute or more; not widely supported
encode(<<"blink_rapid">>)          -> sequence(6);
%% Image: Negative. Swap foreground and background
encode(<<"inverse">>)              -> sequence(7);
%% Image: Negative. Swap foreground and background
encode(<<"reverse">>)              -> encode(<<"inverse">>);
%% Conceal. Not widely supported
encode(<<"conceal">>)              -> sequence(8);
%% Crossed-out. Characters legible, but marked for deletion. Not widely supported.
encode(<<"crosed_out">>)           -> sequence(9);
%% Sets primary (default) font
encode(<<"primary_font">>)         -> sequence(10);
%% Sets alternative font 1
encode(<<"font_1">>)               -> sequence(11);
%% Sets alternative font 2
encode(<<"font_2">>)               -> sequence(12);
%% Sets alternative font 3
encode(<<"font_3">>)               -> sequence(13);
%% Sets alternative font 4
encode(<<"font_4">>)               -> sequence(14);
%% Sets alternative font 5
encode(<<"font_5">>)               -> sequence(15);
%% Sets alternative font 6
encode(<<"font_6">>)               -> sequence(16);
%% Sets alternative font 7
encode(<<"font_7">>)               -> sequence(17);
%% Sets alternative font 8
encode(<<"font_8">>)               -> sequence(18);
%% Sets alternative font 9
encode(<<"font_9">>)               -> sequence(19);
%% Normal color or intensity
encode(<<"normal">>)               -> sequence(22);
%% Not italic
encode(<<"not_italic">>)           -> sequence(23);
%% Underline: None
encode(<<"not_underline">>)        -> sequence(24);
%% Blink: off
encode(<<"blink_off">>)            -> sequence(25);
%% Sets foreground color to black
encode(<<"black">>)                -> sequence(30);
%% Sets foreground color to red
encode(<<"red">>)                  -> sequence(31);
%% Sets foreground color to green
encode(<<"green">>)                -> sequence(32);
%% Sets foreground color to yellow
encode(<<"yellow">>)               -> sequence(33);
%% Sets foreground color to blue
encode(<<"blue">>)                 -> sequence(34);
%% Sets foreground color to magneta
encode(<<"magneta">>)              -> sequence(35);
%% Sets foreground color to cyan
encode(<<"cyan">>)                 -> sequence(36);
%%Sets foreground color to white
encode(<<"white">>)                -> sequence(37);
%% Default text color
encode(<<"default_color">>)        -> sequence(39);
%% Sets background color to black
encode(<<"black_background">>)     -> sequence(40);
%% Sets background color to red
encode(<<"red_background">>)       -> sequence(41);
%% Sets background color to green
encode(<<"green_background">>)     -> sequence(42);
%% Sets background color to yellow
encode(<<"yellow_background">>)    -> sequence(43);
%% Sets background color to blue
encode(<<"blue_background">>)      -> sequence(44);
%% Sets background color to magenta
encode(<<"magneta_background">>)   -> sequence(45);
%% Sets background color to cyan
encode(<<"cyan_background">>)      -> sequence(46);
%% Sets background color to white
encode(<<"white_background">>)     -> sequence(47);
%% Default background color
encode(<<"default_background">>)   -> sequence(49);
%% Framed
encode(<<"framed">>)               -> sequence(51);
%% Encircled
encode(<<"encircled">>)            -> sequence(52);
%% Overlined
encode(<<"overlined">>)            -> sequence(53);
%% Not framed or encircled
encode(<<"not_framed_encircled">>) -> sequence(54);
%% Not overlined
encode(<<"not_overlined">>)        -> sequence(55).

sequence(Value) ->
    "\e[" ++ integer_to_list(Value) ++ "m".
