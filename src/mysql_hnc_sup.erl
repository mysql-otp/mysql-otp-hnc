%% MySQL/OTP + hnc
%% Copyright (C) 2020 Maria Scott <maria-12648430@gmx.net>
%%
%% This file is part of MySQL/OTP + hnc.
%%
%% MySQL/OTP + hnc is free software: you can redistribute it and/or modify it under
%% the terms of the GNU Lesser General Public License as published by the Free
%% Software Foundation, either version 3 of the License, or (at your option)
%% any later version.
%%
%% This program is distributed in the hope that it will be useful, but WITHOUT
%% ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
%% FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for
%% more details.
%%
%% You should have received a copy of the GNU Lesser General Public License
%% along with this program. If not, see <https://www.gnu.org/licenses/>.

-module(mysql_hnc_sup).

-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Pools = application:get_all_env(mysql_hnc),
    Pools1 = proplists:delete(included_applications, Pools),
    {
        ok,
        {
            #{
                strategy => one_for_one
            },
            [mysql_hnc:child_spec(Pool, PoolOptions, MysqlOptions)
             || {Pool, {PoolOptions, MysqlOptions}} <- Pools1]
        }
    }.
