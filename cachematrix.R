# usage:
        #create a matrix and assign it to object x
        #myData <- cacheMtrxInit()
        #setMtrx(myData)<-x
        #getInv(myData)


#initializes a list that contains objects for matrix and its inverse
cacheMtrxInit <-function ()
        list(obj=matrix(0),param = matrix(0))

#reassignment function - compares the existing object from the Mtrx list with one that's passed as "value"
#if the two are different - updates object for matrix and its inverse in Mtrx list by calling "resetMtrx" on Mtrx with value
'setMtrx<-' <- function(Mtrx, value) {
        if (matequal(Mtrx$obj,value) == FALSE) {
                print("matrix updated")
                resetMtrx(Mtrx, value)
                }
        else {
        print("same matrix alread exists")
        Mtrx
        }
}

#assigns new matrix value, computes its inverse and updates Mtrx list        
resetMtrx <-function (Mtrx, value) {
        Mtrx$obj <- value
        Mtrx$param <- solve(value)
        Mtrx
}

#grabs the inverse from the list if the inverse exists
getInv <- function (Mtrx) {
        if (!matequal(Mtrx$param,matrix(0))) Mtrx$param
        else print("matrix is not initialized")
                
}

#matrices comparison         
matequal <- function(x, y)
        is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)
