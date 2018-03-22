# Text file for Shiny UI
# Var names should not contain "." due to conflicts with JavaScript

# TITLE -------------------------------------------------------------------
title_txt <- "Draft stroke risk calculator"


# INPUT UI ------------------------------------------------------------------
in_age <- "Patient's age"
in_sex <- "Patient's sex"
in_stroke <- "Has patient ever had stroke"
in_hf <- "Has patient ever had heart failire"
in_hyperT <- "Has patient ever had hypertension"
in_diab <- "Has patient ever had diabetes"
in_vasc <- "Has patient ever had vascular disease"


# OUTPUT TEXT -------------------------------------------------------------

out_stroke <- "1-year stroke risk is estimated to be:"
out_thomb <- "1-year thomboembolism risk is estimated to be:"

# Text that appears when user has not entered an appropriate age (i.e. <20 ||
# >99 || not a whole number)
enter_age <- "Please enter a whole number between 20 - 99"