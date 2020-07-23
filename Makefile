PROJECT = mysql_hnc
PROJECT_DESCRIPTION = MySQL/OTP + hnc
PROJECT_VERSION = 0.1.0
DEPS = mysql hnc
dep_mysql = git https://github.com/mysql-otp/mysql-otp.git master
dep_hnc = git https://github.com/juhlig/hnc.git 0.1.0
include erlang.mk
