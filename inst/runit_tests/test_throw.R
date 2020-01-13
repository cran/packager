test_exception <- function() {
    RUnit::checkException(packager:::throw("Hello, error!"))
}
