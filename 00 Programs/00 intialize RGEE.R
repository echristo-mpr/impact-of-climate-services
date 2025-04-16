install.packages("rgee")
install.packages("reticulate")

library(rgee)
library(reticulate)

ee_install_set_pyenv(Renviron = "local", py_path = "C:\\Users\\echristo\\AppData\\Local\\anaconda3\\envs\\rgee_39", py_env = "rgee_39")

ee_Initialize(user = 'echristo17@gmail.com',
              drive = TRUE)

AOI <- ee$Geometry$Point(5.25, 13.79)

