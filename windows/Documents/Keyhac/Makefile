mkfile_dir := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))

pip_python := 3.8
pip_platform := win32
pip_target := ${mkfile_dir}/extension/vendor
pip_requirements := ${mkfile_dir}/requirements.txt

.PHONY: vendor

vendor:
ifndef $(pip_platform)
	pip install --python-version=$(pip_python) --only-binary=:all: -t $(pip_target) -r $(pip_requirements) -U
else
	pip install --python-version=$(pip_python) --platform=$(pip_platform) --only-binary=:all: -t $(pip_target) -r $(pip_requirements) -U
endif
