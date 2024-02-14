getPkgDir <- function(){
    tdir <- file.path(tempdir(), "packages")
    dir.create(tdir)
    return(tdir)
}

cleanup <- function(tdir){
    if(dir.exists(tdir)){
        unlink(
            file.path(tdir),
            recursive=T
        )
    }
}
