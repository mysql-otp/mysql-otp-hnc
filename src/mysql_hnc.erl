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

-module(mysql_hnc).

-export([add_pool/3, remove_pool/1,
         checkin/2, checkout/1,
         child_spec/3,
         execute/3, execute/4,
         query/2, query/3, query/4,
         transaction/2, transaction/3, transaction/4,
         with/2]).

%% @doc Adds a pool to the started mysql_hnc application.
-spec add_pool(Pool, PoolOptions, MysqlOptions) -> {ok, pid()}
        when Pool :: hnc:pool(),
             PoolOptions :: hnc:opts(),
             MysqlOptions :: [mysql:option()].
add_pool(Pool, PoolOptions, MysqlOptions) ->
    %% We want strategy fifo as default instead of lifo.
    PoolOptions1 = case maps:is_key(strategy, PoolOptions) of
        true -> PoolOptions;
        false -> PoolOptions#{strategy => lifo}
    end,
    MysqlOptions1 = case lists:keymember(connect_mode, 1, MysqlOptions) of
        true -> MysqlOptions;
        false -> [{connect_mode, lazy}|MysqlOptions]
    end,
    PoolSpec = child_spec(Pool, PoolOptions1, MysqlOptions1),
    supervisor:start_child(mysql_hnc_sup, PoolSpec).

%% @doc Stops and removes the pool from the started mysql_hnc application.
-spec remove_pool(Pool) -> ok | {error, not_found}
        when Pool :: hnc:pool().
remove_pool(Pool) ->
    case supervisor:terminate_child(mysql_hnc_sup, {hnc_embedded_sup, Pool}) of
        ok -> supervisor:delete_child(mysql_hnc_sup, {hnc_embedded_sup, Pool});
        Error -> Error
    end.

%% @doc Returns a mysql connection to the given pool.
-spec checkin(Pool, Conn) -> ok
        when Pool :: hnc:pool(),
             Conn :: mysql:connection().
checkin(Pool, Conn) ->
    hnc:checkin(Pool, Conn).

%% @doc Checks out a mysql connection from a given pool.
-spec checkout(Pool) -> Conn
        when Pool :: hnc:pool(),
             Conn :: mysql:connection().
checkout(Pool) ->
    hnc:checkout(Pool).

%% @doc Creates a supvervisor:child_spec. When the need to
%% supervise the pools in another way.
-spec child_spec(Pool, PoolOptions, MysqlOptions) -> ChildSpec
        when Pool :: hnc:pool(),
             PoolOptions :: hnc:opts(),
             MysqlOptions :: [mysql:option()],
             ChildSpec :: supervisor:child_spec().
child_spec(Pool, PoolOptions, MysqlOptions) ->
    hnc:child_spec(Pool, PoolOptions, mysql, MysqlOptions).

%% @doc Execute a mysql prepared statement with given params.
-spec execute(Pool, StatementRef, Params) -> Result
        when Pool :: hnc:pool(),
             StatementRef :: mysql:statement_ref(),
             Params :: [mysql:query_param()],
             Result :: mysql:query_result().
execute(Pool, StatementRef, Params) ->
    hnc:transaction(Pool, fun(MysqlConn) ->
        mysql:execute(MysqlConn, StatementRef, Params)
    end).

%% @doc Execute a mysql prepared statement with given params and timeout
-spec execute(Pool, StatementRef, Params, Timeout) -> Result
        when Pool :: hnc:pool(),
             StatementRef :: mysql:statement_ref(),
             Params :: [mysql:query_param()],
             Timeout :: timeout(),
             Result :: mysql:query_result();
     (Pool, StatementRef, Params, FilterMap) -> Result
        when Pool :: hnc:pool(),
             StatementRef :: mysql:statement_ref(),
             Params :: [mysql:query_param()],
             FilterMap :: mysql:query_filtermap_fun(),
             Result :: mysql:query_result().
execute(Pool, StatementRef, Params, FiltermapOrTimeout) ->
    hnc:transaction(Pool, fun(MysqlConn) ->
        mysql:execute(MysqlConn, StatementRef, Params, FiltermapOrTimeout)
    end).

%% @doc Executes a query to a mysql connection in a given pool.
-spec query(Pool, Query) -> Result
        when Pool :: hnc:pool(),
             Query :: mysql:query(),
             Result :: mysql:query_result().
query(Pool, Query) ->
    hnc:transaction(Pool, fun(MysqlConn) ->
        mysql:query(MysqlConn, Query)
    end).

%% @doc Executes a query to a mysql connection in a given pool with either
%% list of query parameters or a timeout value.
-spec query(Pool, Query, Params | FilterMap | Timeout) -> Result
        when Pool :: hnc:pool(),
             Query :: mysql:query(),
             Params :: [mysql:query_param()],
             FilterMap :: mysql:query_filtermap_fun(),
             Timeout :: timeout(),
             Result :: mysql:query_result().
query(Pool, Query, ParamsOrFiltermapOrTimeout) ->
    hnc:transaction(Pool, fun(MysqlConn) ->
        mysql:query(MysqlConn, Query, ParamsOrFiltermapOrTimeout)
    end).

%% @doc Executes a query to a mysql connection in a given pool with both
%% a list of query parameters and a timeout value.
-spec query(Pool, Query, Params, Timeout) -> Result
        when Pool :: hnc:pool(),
             Query :: mysql:query(),
             Params :: [mysql:query_param()],
             Timeout :: timeout(),
             Result :: mysql:query_result();
    (Pool, Query, FilterMap, Timeout) -> Result
        when Pool :: hnc:pool(),
             Query :: mysql:query(),
             FilterMap :: mysql:query_filtermap_fun(),
             Timeout :: timeout(),
             Result :: mysql:query_result();
    (Pool, Query, Params, FilterMap) -> Result
        when Pool :: hnc:pool(),
             Query :: mysql:query(),
             Params :: [mysql:query_param()],
             FilterMap :: mysql:query_filtermap_fun(),
             Result :: mysql:query_result().
query(Pool, Query, ParamsOrFiltermap, FiltermapOrTimeout) ->
    hnc:transaction(Pool, fun(MysqlConn) ->
        mysql:query(MysqlConn, Query, ParamsOrFiltermap, FiltermapOrTimeout)
    end).

%% @doc Wrapper to hnc:transaction/2. Since it is not a mysql transaction.
%% Example instead of:
%% Conn = mysql_hnc:checkout(mypool),
%% try
%%     mysql:query(Conn, "SELECT...")
%%  after
%%     mysql_hnc:checkin(mypool, Conn)
%%  end.
%%
%% mysql_hnc:with(mypool, fun (Conn) -> mysql:query(Conn, "SELECT...") end).
-spec with(Pool, Fun) -> Result
        when Pool :: hnc:pool(),
             Fun :: hnc:transaction_fun(Result),
             Result :: term().
with(Pool, Fun) when is_function(Fun, 1) ->
    hnc:transaction(Pool, Fun).

%% @doc Executes a mysql transaction fun. The fun needs to take one argument
%% which is the mysql connection.
-spec transaction(Pool, TransactionFun) -> Result
        when Pool :: hnc:pool(),
             TransactionFun :: fun(),
             Result :: {atomic, term()} | {aborted, term()}.
transaction(Pool, TransactionFun) when is_function(TransactionFun, 1) ->
    hnc:transaction(Pool, fun(MysqlConn) ->
        mysql:transaction(MysqlConn, TransactionFun, [MysqlConn], infinity)
    end).

%% @doc Executes a transaction fun. Args list needs be the same length as
%% TransactionFun arity - 1.
-spec transaction(Pool, TransactionFun, Args) -> Result
        when Pool :: hnc:pool(),
             TransactionFun :: fun(),
             Args :: [term()],
             Result :: {atomic, term()} | {aborted, term()}.
transaction(Pool, TransactionFun, Args)
    when is_function(TransactionFun, length(Args) + 1) ->
    hnc:transaction(Pool, fun(MysqlConn) ->
        mysql:transaction(MysqlConn, TransactionFun, [MysqlConn | Args],
                          infinity)
    end).

%% @doc Same as transaction/3 but with the number of retries the mysql
%% transaction should try to execute.
-spec transaction(Pool, TransactionFun, Args, Retries) -> Result
        when Pool :: hnc:pool(),
             TransactionFun :: fun(),
             Args :: [term()],
             Retries :: non_neg_integer() | infinity,
             Result :: {atomic, term()} | {aborted, term()}.
transaction(Pool, TransactionFun, Args, Retries)
    when is_function(TransactionFun, length(Args) + 1) ->
    hnc:transaction(Pool, fun(MysqlConn) ->
        mysql:transaction(MysqlConn, TransactionFun, [MysqlConn | Args],
                          Retries)
    end).
