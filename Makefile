PROJECT = yaws_api_ext
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

NO_AUTOPATCH += yaws
DEPS = yaws
dep_yaws = git https://github.com/klacke/yaws master

include erlang.mk
