sim_globe <- function(p=0.7,N=9){
        sample(c("W","L"),
               size=N,
               prob = c(p,1-p),
               replace = TRUE)
}
sim_globe()
