# dataraft.adapters 0.1.0.9005

* Quality metadata uses canonical action/threshold rule state.
* The adapter conformance protocol has five capabilities; partition is removed.

# dataraft.adapters 0.1.0.9004

* Own catalog, OpenLineage, OpenMetadata and catalog viewer implementations previously in dataraft.catalog.
* Add optional failure-injection conformance probes and document atomicity, retry and snapshot obligations.

* Use the umbrella CI manifest as the single immutable family dependency lock.

# dataraft.adapters 0.1.0.9000

* `dr_target_iceberg()` explicitly declares experimental, ungoverned writes and validates stored candidates lazily instead of collecting entire tables.
* `dr_test_adapter()` checks extension protocol declarations and optional disposable-target roundtrips.

* Add ODCS 3.2 interchange with a documented executable subset, safe YAML project manifests and experimental DuckDB Iceberg source/REST target adapters.

* Keep stateless helpers private and prefix shared implementation interfaces with `dr_internal_`. Move component tests into their owning repository; add minimal and downstream CI.

* Keep database connection failures distinguishable as backend errors. Add independent pins version-reference tests and an optional pins CI job.

* Initial independent DataRaft package.

* Add versioned local `dr_target_rds()` and `dr_source_rds()` without an optional storage engine.
