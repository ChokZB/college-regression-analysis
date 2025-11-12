# ------------------------------------------------------------
# Script: install_packages.R
# Purpose: Install all required R packages for this project
# Author: Chok Zu Bing
# ------------------------------------------------------------

required_packages <- c(
    "ISLR2", "ggplot2", "dplyr", "corrplot", "leaps", "boot", "caret"
)

for (pkg in required_packages) {
    if (!require(pkg, character.only = TRUE)) {
        install.packages(pkg)
    }
}