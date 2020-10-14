%%%-------------------------------------------------------------------
%%% @author Razvan.Lupu
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Oct 2020 20:50
%%%-------------------------------------------------------------------
-module(http_server_api).
-author("Razvan.Lupu").

-include("./../include/http_server_api.hrl").

%% API
-export([process/1, sort/1]).

-define(flog(Fmt, Args), begin
                           file:write_file("/tmp/flog.txt", lists:flatten(io_lib:format("~p/~p: " ++ Fmt, [?MODULE, ?LINE | Args])), [append])
                         end).

process(Req) ->
  Body = mochiweb_request:recv_body(Req),
  {struct, ReqParams} = mochijson2:decode(Body),

  Tasks = proplists:get_value(?TASKS, ReqParams, []),
  TaskTuples = lists:map(
      fun(TaskProplist) ->
        Name     = proplists:get_value(?COMMAND_NAME, TaskProplist),
        Command  = proplists:get_value(?COMMAND, TaskProplist),
        Requires = proplists:get_value(?REQUIRES, TaskProplist, []),
        {Name, Command, Requires}
      end, [X || {struct, X} <- Tasks]),

  Valid = validate(TaskTuples) andalso TaskTuples =/= [],

  case Valid of
    false -> bad_request(Req);
    _ ->
      SortedTasks = sort(TaskTuples),
      execute(SortedTasks),
      write(SortedTasks),
      Response =
        {struct, [{?TASKS,
          [ case R of
              [] ->
                {struct, [{?COMMAND_NAME, N}, {?COMMAND, C}]};
              _ ->
                {struct, [{?COMMAND_NAME, N}, {?COMMAND, C}, {?REQUIRES, R}]}
            end || {N, C, R} <- SortedTasks]
        }]},

      mochiweb_request:respond({200, [{"Content-Type", "application/json"}], mochijson2:encode(Response)} , Req)
  end.

bad_request(Req) ->
  mochiweb_request:respond({400, [{"Content-Type", "text/plain"}], "Bad Request."} , Req).

execute([{_,Cmd,_} | T]) ->
  os:cmd(binary_to_list(Cmd)),
  execute(T);
execute([]) ->
  ok.

write(Tasks) ->
  L0 = lists:map(fun({_, Cmd, _}) -> binary_to_list(Cmd) end, Tasks),
  L = ["#!/bin/bash\n"] ++ L0,
  Body = string:join(L, "\n"),
  os:cmd("echo \"" ++ Body ++ "\" > exec.sh").

validate([{undefined, _, _} | _T]) ->
  false;
validate([{_, undefined, _} | _T]) ->
  false;
validate([_H | T]) ->
  validate(T);
validate([]) ->
  true.

sort(Tasks) ->
  sort2([], Tasks).

sort2(Acc, []) ->
  Acc;
sort2(Acc0, Tasks) ->
  Executed =
    lists:foldl(fun({Name, Command, Requires}, Acc) ->
      Queue = [N || {N, _, _}  <- Acc],
      case Requires of
        [] ->
          Acc ++ [{Name, Command, Requires}];
        L ->
          case L -- Queue of
            [] -> Acc ++ [{Name, Command, Requires}];
            _  -> Acc
          end
      end
                end, Acc0, Tasks),
  sort2(Executed, Tasks -- Executed).