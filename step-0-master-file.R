
##
### 1. Generate working directories 
##

the.data.path <- c("")
the.plot.path <- c("")

##
### 2. Source files step by step
##

setwd(the.data.path)

## Recommend to open and run each script separately (to see what is happening)

source("step-0-basic-functions.R")
source("step-1-a-load-data.R")
source("step-2-infections-map-ifr-thanatAge-ageSpecific-deaths.R")
source("step-3-infections-map-ifr-chronAge-ageSpecific-deaths.R")
source("step-4-confirmed-deaths.R")

## To calculate / update global death distribution by age: 
## source("step-1-b-get-global-death-distribution-by-age.R")

