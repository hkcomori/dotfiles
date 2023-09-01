mkfile_dir := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))

pip_target := ${mkfile_dir}/extension/vendor
pip_requirements := ${mkfile_dir}/requirements.txt

.PHONY: vendor

vendor:
	pip install -t ${pip_target} -r ${pip_requirements} -U
