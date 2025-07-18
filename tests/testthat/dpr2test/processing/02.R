library(DPR2)
mymatrix <- matrix(1:16, nrow=4)
mydataframe <- data.frame(x=1:3, y=LETTERS[1:3])
dpr_save('mymatrix')
