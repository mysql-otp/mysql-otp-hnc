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
{application, mysql_hnc, [
    {description, "MySQL/OTP + hnc"},
    {vsn, "0.2.0"},
    {modules, ['mysql_hnc','mysql_hnc_app','mysql_hnc_sup']},
    {mod, {mysql_hnc_app, []}},
    {applications, [kernel, stdlib, mysql, hnc]}
]}.
