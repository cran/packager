test_exception <- function() {
    RUnit::checkException({{{package}}}:::throw("Hello, error!"))
}
