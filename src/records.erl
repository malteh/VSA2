-module(records).
-compile(export_all).

-record(
	node, {
		name,
		status=sleeping,
		level=0,
		basic_edges=[],
		rejected_edges=[],
		branch_edges=[],
		core,
		parent,
		min_child,
		deferred_tests=[],
		mwoe
	}).