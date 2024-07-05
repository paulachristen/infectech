library(covr)
report <- covr::package_coverage()
covr::report(report, file = "coverage.md")
