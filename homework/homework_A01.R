install.packages(c("coda","mvtnorm","devtools","dagitty"))
install.packages("cmdstanr", repos = c('https://stan-dev.r-universe.dev', getOption("repos")))
library(devtools)
devtools::install_github("rmcelreath/rethinking")


# ---- 2E1 ----
## 2->Pr(rain|Monday) & 4 -> Pr(rain, Monday)/Pr(Monday)
# ---- 2E2 ----
## 3-> the probability that it's Monday given that it's raining
# ---- 2E3 ----
## 1->Pr(Monday|rain) and 4-> Pr(rain|Monday) Pr(Monday)/ Pr(rain)
# ---- 2E4 ----
## The probability of water is 0.7 means that water is going to happen more often
## than land when tossing the globe multiple times
# ---- 2M1 ----
p_grid <- seq(from=0, to=1, length.out=20)
post <- function(waters,total,prior){
        likelihood <- dbinom(waters, size=total, prob=p_grid)
        unstd.posterior <- likelihood * prior
        return(unstd.posterior / sum(unstd.posterior))
}

plot(p_grid, post(3,3, rep(1,20)), type="b")
plot(p_grid, post(3,4, rep(1,20)), type="b")
plot(p_grid, post(5,7, rep(1,20)), type="b")
# ---- 2M2 ----
prior_2m2 <- ifelse(p_grid<0.5,0,2)
plot(p_grid, post(3,3, prior_2m2), type="b")
plot(p_grid, post(3,4, prior_2m2), type="b")
plot(p_grid, post(5,7, prior_2m2), type="b")
# ---- 2M3 ----
## Pr(x|y)=Pr(y|x)*Pr(x)/Pr(y) ->Pr(E|L)=Pr(L|E)*P(E)/P(L)=0.3*0.5/P(L)
## P(L)=P(L|E)*P(E)+P(L|M)*P(M)=0.3*0.5+1*0.5
print(paste("the solution is",0.3*0.5/(0.3*0.5+1*0.5)))
# ---- 2M4 ----
## If we pull card A
### side 1: black -> Other side: black
### side 2: black -> Other side: black
## If we pull card B
### side 1: black -> Other side: white
### side 2: white
## If we pull card C
### side 1: white
### side 2: white
## If we pull a card and it's black, probability of other side being black is 2/3
# ---- 2M5 ----
# ---- 2M6 ----
# ---- 2M7 ----
# ---- 2H1 ----
# ---- 2H2 ----
# ---- 2H3 ----
# ---- 2H4 ----
# ---- A1 ----
