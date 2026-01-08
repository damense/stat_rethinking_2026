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
## If we pull card A
### side 1: black -> Other side: black
### side 2: black -> Other side: black
## If we pull card D
### side 1: black -> Other side: black
### side 2: black -> Other side: black
## If we pull card B
### side 1: black -> Other side: white
### side 2: white
## If we pull card C
### side 1: white
### side 2: white
## If we pull a card and it's black, probability of other side being black is 4/5
# ---- 2M6 ----
## If we pull card A
### side 1: black -> Other side: black
### side 2: black -> Other side: black
## If we pull card B
### side 1: black -> Other side: white
### side 2: white
## If we pull card B
### side 1: black -> Other side: white
### side 2: white
## If we pull card C
### side 1: white
### side 2: white
## If we pull card C
### side 1: white
### side 2: white
## If we pull card C
### side 1: white
### side 2: white
## If we pull a card and it's black, probability of other side being black is 2/4
# ---- 2M7 ----
## I did it in the whiteboard but there are 8 possibilities of first card with a black side and second card with a white side. 6 of them have black on the side of the other card 3/4=0.75
# ---- 2H1 ----
# ---- 2H2 ----
# ---- 2H3 ----
# ---- 2H4 ----
# ---- A1 ----
library(tidyverse)
library(gtools)
n_people <- 10
values <- c(T,F)
res <-as.data.frame(
        permutations(n=length(values),
                     r=n_people,
                     v=values,
                     repeats.allowed = T)
)
# Using the “garden of forking data” approach, how many ways are there to realize
# this sample (8 out of 10), if all participants are honest?
res %>%
        mutate(sum = as.factor(rowSums(., na.rm=TRUE))) %>% 
        group_by(sum) %>% 
        summarise(n=n()) %>% 
        mutate(freq=n/sum(n))

### There are 45 out of 1024 ways = 0.0439 to realize (8 out of 10)


# How many ways, if 5 of the participants are honest and the other 3 lie if the have a False?
###If 5 participants are honest and the other five say they got "heads"


v_cols <- paste0("V", seq_len(5))

res %>%
        mutate(across(all_of(v_cols), ~ TRUE)) %>%
        mutate(sum = rowSums(across(starts_with("V")), na.rm = TRUE)) %>%
        count(sum, name = "n") %>%
        mutate(freq = n / sum(n)) %>%
        filter(sum == 8) %>%
        pull(n) %>%
        { if (length(.) == 0) 0 else . }

### The amount is 320/1024=0.3125
# Can you figure out the number of honest participants that maximizes the number of ways to realize the observed sample
#(8 out of 10)
map_dbl(
        seq_len(10),
        function(k) {
                v_cols <- paste0("V", seq_len(k))
                
                res %>%
                        mutate(across(all_of(v_cols), ~ TRUE)) %>%
                        mutate(sum = rowSums(across(starts_with("V")), na.rm = TRUE)) %>%
                        count(sum, name = "n") %>%
                        mutate(freq = n / sum(n)) %>%
                        filter(sum == 8) %>%
                        pull(n) %>%
                        { if (length(.) == 0) 0 else . }
        }
)
### The amount is 6