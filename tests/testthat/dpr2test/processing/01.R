library(yaml)
library(lubridate)
library(DPR2)
date('2024-01-01') # test function masking of `date()`
mydataframe <- data.frame(x=1:10, y=LETTERS[1:10])
myotherdata <- mydataframe
myyaml <- as.yaml(data.frame(a=1:3))
objYml1 <- 'test objects values 1'
objYml2 <- 'test objects values 2'
dpr_save('mydataframe')
dpr_save('myotherdata')
# Using save() is not a best practice, but needed for a unit test.
# use dpr_save() instead.
save(myyaml, file=dpr_path('data', 'myyaml.rda'))
