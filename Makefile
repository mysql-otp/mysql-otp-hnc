PROJECT = mysql_hnc
PROJECT_DESCRIPTION = MySQL/OTP + hnc
PROJECT_VERSION = 0.3.0
DEPS = mysql hnc
dep_mysql = git https://github.com/mysql-otp/mysql-otp.git 1.7.0
dep_hnc = git https://github.com/hnc-agency/hnc.git 0.3.0
include erlang.mk
