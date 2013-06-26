-module(cli).
-export([puts/1, puts/2]).

%%
%%  >> cli:puts(<<"ala ma %{reset, red} kota%{reset}">>, standard_io).
%%
puts(BitString) ->
    puts(BitString, standard_io).
puts(BitString, Device) ->
    io:put_chars(Device, [cli_ansi:escape(BitString), $\n]).


