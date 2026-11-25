# Register the built-in handlers again before the tests run. They are registered once in
# .onLoad(), and covr does not count code that runs at load time, so the relation closures
# the registry holds would otherwise show as untested although every check_reported() test
# runs them. Registration is idempotent (see test-infra-registry.R).
register_builtin_handlers()
