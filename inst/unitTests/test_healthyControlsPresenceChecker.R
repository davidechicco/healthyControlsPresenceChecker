# test_healthyControlsCheck <- function() {
test_healthyControlsCheck <- function() {
    checkEquals(healthyControlsCheck("GSE19429", FALSE), TRUE)
    checkEquals(healthyControlsCheck("GSE58831", FALSE), TRUE)
    checkEquals(healthyControlsCheck("GSE19429", FALSE), TRUE)
    checkEquals(healthyControlsCheck("GSE34111", FALSE), TRUE)
    checkEquals(healthyControlsCheck("GSE47407", FALSE), FALSE)
    checkEquals(healthyControlsCheck("GSE32123131231", FALSE), NULL)
}


