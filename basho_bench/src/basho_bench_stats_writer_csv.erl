%% -------------------------------------------------------------------
%%
%% basho_bench: Benchmarking Suite
%%
%% Copyright (c) 2009-2014 Basho Techonologies
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
%% HOWTO:
%%
%% * To run basho_bench with the default CSV writer, nothing needs to
%%   be done. But if wanting to override a former setting, then
%%   writing the following in the benchmark config file will switch
%%   the stats writer to CSV:
%%
%%    {stats, {csv}}.
%%.

-module(basho_bench_stats_writer_csv).

%% For getting the total list of percentiles (not in percentage)
-define(PERCENTILE_STEP, 0.01).
-define(PERCENTILE_MAX, 1).

-export([new/2,
         terminate/1,
         process_summary/5,
         report_error/3,
         report_latency/7]).

-include("basho_bench.hrl").

new(Ops, Measurements) ->
    ?INFO("module=~s event=start stats_sink=csv\n", [?MODULE]),
    %% Setup output file handles for dumping periodic CSV of histogram results.
    [erlang:put({csv_file, X}, op_csv_file(X)) || X <- Ops],

    %% Setup output file handles for dumping periodic CSV of histogram results.
    [erlang:put({csv_file, X}, measurement_csv_file(X)) || X <- Measurements],

    %% Setup output file w/ counters for total requests, errors, etc.
    {ok, SummaryFile} = file:open("summary.csv", [raw, binary, write]),
    file:write(SummaryFile, <<"elapsed, window, total, successful, failed\n">>),

    %% Setup errors file w/counters for each error.  Embedded commas likely
    %% in the error messages so quote the columns.
    {ok, ErrorsFile} = file:open("errors.csv", [raw, binary, write]),
    file:write(ErrorsFile, <<"\"error\",\"count\"\n">>),

    {SummaryFile, ErrorsFile}.

terminate({SummaryFile, ErrorsFile}) ->
    ?INFO("module=~s event=stop stats_sink=csv\n", [?MODULE]),
    [ok = file:close(F) || {{csv_file, _}, F} <- erlang:get()],
    ok = file:close(SummaryFile),
    ok = file:close(ErrorsFile),
    ok.

process_summary({SummaryFile, _ErrorsFile},
                Elapsed, Window, Oks, Errors) ->
    file:write(SummaryFile,
               io_lib:format("~w, ~w, ~w, ~w, ~w\n",
                             [Elapsed,
                              Window,
                              Oks + Errors,
                              Oks,
                              Errors])).

report_error({_SummaryFile, ErrorsFile},
             Key, Count) ->
    file:write(ErrorsFile,
               io_lib:format("\"~w\",\"~w\"\n",
                             [Key, Count])).

report_latency({_SummaryFile, _ErrorsFile, RemoteLatenciesList},
               Elapsed, Window, Op,
               Stats, Errors, Units) ->
    case proplists:get_value(n, Stats) > 0 of
        true ->
            P = proplists:get_value(percentile, Stats),

            %%%%%%%%%%%%%%%%%%%%%%%%%
            %% CHANGED THIS BRANCH %%
            %%%%%%%%%%%%%%%%%%%%%%%%%
            case Op of
                {remote_read, remote_read} ->
                    %% Build the line to output
                    Line = format_percentiles_line(Elapsed,
                                                   Window,
                                                   Units,
                                                   Stats,
                                                   P,
                                                   RemoteLatenciesList,
                                                   Errors);

                %% Otherwise do the same as the original
                _ ->
                    Line = io_lib:format("~w, ~w, ~w, ~w, ~.1f, ~w, ~w, ~w, ~w, ~w, ~w, ~w\n",
                                        [Elapsed,
                                        Window,
                                        Units,
                                        proplists:get_value(min, Stats),
                                        proplists:get_value(arithmetic_mean, Stats),
                                        proplists:get_value(median, Stats),
                                        proplists:get_value(90, P),
                                        proplists:get_value(95, P),
                                        proplists:get_value(99, P),
                                        proplists:get_value(999, P),
                                        proplists:get_value(max, Stats),
                                        Errors])
            end;
        false ->
            ?WARN("No data for op: ~p\n", [Op]),
            Line = io_lib:format("~w, ~w, 0, 0, 0, 0, 0, 0, 0, 0, ~w\n",
                                 [Elapsed,
                                  Window,
                                  Errors])
    end,
    file:write(erlang:get({csv_file, Op}), Line).

%% ====================================================================
%% Internal functions
%% ====================================================================

op_csv_file({Label, _Op}) ->
    Fname = normalize_label(Label) ++ "_latencies.csv",
    {ok, F} = file:open(Fname, [raw, binary, write]),
    ok = file:write(F, <<"elapsed, window, n, min, mean, median, 95th, 99th, 99_9th, max, errors\n">>),
    F.

measurement_csv_file({Label, _Op}) ->
    Fname = normalize_label(Label) ++ "_measurements.csv",
    {ok, F} = file:open(Fname, [raw, binary, write]),
    ok = file:write(F, <<"elapsed, window, n, min, mean, median, 95th, 99th, 99_9th, max, errors\n">>),
    F.

normalize_label(Label) when is_list(Label) ->
    replace_special_chars(Label);
normalize_label(Label) when is_binary(Label) ->
    normalize_label(binary_to_list(Label));
normalize_label(Label) when is_integer(Label) ->
    normalize_label(integer_to_list(Label));
normalize_label(Label) when is_atom(Label) ->
    normalize_label(atom_to_list(Label));
normalize_label(Label) when is_tuple(Label) ->
    Parts = [normalize_label(X) || X <- tuple_to_list(Label)],
    string:join(Parts, "-").

replace_special_chars([H|T]) when
      (H >= $0 andalso H =< $9) orelse
      (H >= $A andalso H =< $Z) orelse
      (H >= $a andalso H =< $z) ->
    [H|replace_special_chars(T)];
replace_special_chars([_|T]) ->
    [$-|replace_special_chars(T)];
replace_special_chars([]) ->
    [].


%% Added for giving the complete list of percentiles
new_line(String, List, RemoteLatenciesList) ->
    Percentiles = get_percentiles(RemoteLatenciesList),
    new_line_tail(String, List, Percentiles, 1).

new_line_tail(String, List, _, 101) ->
    {String, List};

new_line_tail(String, List, Percentiles, Next) ->
    new_line_tail(String ++ ", ~w",
                  List ++ [lists:nth(Next, Percentiles)],
                  Percentiles,
                  Next + 1).

%% Returns a list with 
get_percentiles(RemoteLatenciesList) ->
    SortedList = lists:sort(RemoteLatenciesList),
    Size = length(SortedList),
    get_percentiles_tail(SortedList, Size, ?PERCENTILE_STEP, []).

get_percentiles_tail(SortedList, Size, Percentage0, Percentiles) ->
    
    %% Due to the number representation, it avoids some errors
    Percentage1=round_percentage(Percentage0),

    case Percentage1 < ?PERCENTILE_MAX of
        true ->
            Index0 = Size * Percentage1,

            %% Check if the index is a whole number and get the final one
            Value = case is_integer(Index0) of
                true ->
                    (lists:nth(Index0, SortedList) + lists:nth(Index0 + 1, SortedList)) / 2;
                false ->
                    %% Indexes start at 1
                    Index1 = case Index0 < 1 of
                        true ->
                            1;
                        false ->
                            round(Index0)
                    end,
                    lists:nth(Index1, SortedList)
            end,
            get_percentiles_tail(SortedList,
                                Size,
                                Percentage1 + ?PERCENTILE_STEP,
                                Percentiles ++ [Value]);
        false ->
            Percentiles ++ [lists:nth(Size, SortedList)]
    end.

%% To round a number to 2 decimal digits
round_percentage(Percentage) ->
    round(100 * Percentage) / 100.

%% To abstract the build of the line to output
format_percentiles_line(Elapsed, Window, Units, Stats, P, OpList, Errors) ->
    %% Add the missing percentiles
    {LineFormat0, ListFormat0} = new_line("~w, ~w, ~w, ~w, ~.1f, ~w, ~w, ~w, ~w, ~w",
                                        [Elapsed,
                                        Window,
                                        Units,
                                        proplists:get_value(min, Stats),
                                        proplists:get_value(arithmetic_mean, Stats),
                                        proplists:get_value(median, Stats),
                                        proplists:get_value(95, P),
                                        proplists:get_value(99, P),
                                        proplists:get_value(999, P),
                                        proplists:get_value(max, Stats)],
                                        OpList),

    %% Add maximum value, errors and new line character
    LineFormat1 = LineFormat0 ++ ", ~w\n",
    ListFormat1 = ListFormat0 ++ [Errors],

    %% Print line
    io_lib:format(LineFormat1, ListFormat1).
