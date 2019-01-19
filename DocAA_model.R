#load required packages
require(readxl)
require(data.table)
require(ggplot2)
require(abind)
require(dampack)
require(reshape2)
require(lhs)
require(scales)
require(matrixStats)
require(pbapply)
require(car)
require(mvtnorm)
require(ellipse)
require(grid)


######################### INPUT PARAMETERS ####################################

v.n  <- c("hspc", "crpc", "pca.death", "all.death") # state names
n.s  <- length(v.n)                                 # number of states
n.t  <- 200                                        # number of cycles (months)
age <- 60
d.r <- 0.03/12
v.dw <- 1 / (1 + d.r) ^ (0:n.t) # calculate discount weight for each cycle based on discount rate d.r

life.table <- as.data.frame(read_xlsx("life.table.months.xlsx")) # read in life table (converted to rate per month)

## utilities
u.hspc <- 0.83/12
u.doc <- u.hspc - 1/30
u.aa <- 0.865/12
u.crpc <- 0.45/12
u.ctxadmin <- -1/30

## costs
c.hspc <- 489
c.doc <- 550
c.doc.adm <- 361
c.aa <- 9399 
c.crpc <- 4887
c.doc.tox <- 1258
c.aa.tox <- 568
c.pca.death <- 35003

#toxicity
u.neutropenia <- 0.09
c.neutropenia <- 16128

u.febneutropenia <- 0.37
c.febneutropenia <- 26955

u.neutropeniainfection <- 0.4
c.neutropeniainfection <- 36759

u.anaemia <- 0.2
c.anaemia <- 21383

u.thrombocytopenia <- 0.053
c.thrombocytopenia <- 14789

u.diarrhoea <- 0.103
c.diarrhoea <- 17425

u.sensneuro <- 0.175
c.sensneuro <- 29980

u.periphoedema <- 0.06
c.periphoedema <- 26773

u.dyspnoea <- 0.1
c.dyspnoea <- 22172

u.stomatitis <- 0.151
c.stomatitis <- 19157

u.mucositis <- 0.151
c.mucositis <- 19157

u.alt <- 0.05
c.alt <- 6867

u.ast <- 0.05
c.ast <- 6867

u.backpain <- 0.11
c.backpain <- 4121

u.cardiac <- 0.26
c.cardiac <- 22604

u.af <- 0.164
c.af <- 6766

u.hypok <- 0.04
c.hypok <- 6694

#toxicity incidence
i.neutropenia.doc <- 0.32/6
i.neutropenia.aa <- 0

i.febneutropenia.doc <- 0.07/6
i.febneutropenia.aa <- 0

i.neutropeniainfection.doc <- 0.02/6
i.neutropeniainfection.aa <- 0

i.anaemia.doc <- 0.02/6
i.anaemia.aa <- 0.03/6

i.thrombocytopenia.doc  <- 0.006/6
i.thrombocytopenia.aa <- 0

i.diarrhoea.doc <- 0.006/6
i.diarrhoea.aa <- 0

i.sensneuro.doc <- 0.02/6
i.sensneuro.aa <- 0

i.periphoedema.doc <- 0.01/6
i.periphoedema.aa <- 0

i.dyspnoea.doc <- 0.02/6
i.dyspnoea.aa <- 0

i.stomatitis.doc <- 0.006/6
i.stomatitis.aa <- 0

i.mucositis.doc <- 0.006/6
i.mucositis.aa <- 0

i.alt.doc <- 0.02/6
i.alt.aa <-  0.052/6

i.ast.doc <- 0.02/6
i.ast.aa <- 0.045/6

i.backpain.doc <- 0
i.backpain.aa <- 0.02/6

i.cardiac.doc <- 0
i.cardiac.aa <- 0.04/6

i.af.doc <- 0
i.af.aa <- 0.05/6

i.hypok.doc <- 0
i.hypok.aa <- 0.11/6

#toxicities
u.tox.doc <-  (u.neutropenia * i.neutropenia.doc + u.febneutropenia * i.febneutropenia.doc + u.neutropeniainfection * i.neutropeniainfection.doc + u.anaemia * i.anaemia.doc + u.thrombocytopenia * i.thrombocytopenia.doc + u.diarrhoea * i.diarrhoea.doc + u.sensneuro * i.sensneuro.doc + u.periphoedema * i.periphoedema.doc + u.dyspnoea * i.dyspnoea.doc + u.stomatitis * i.stomatitis.doc + u.mucositis * i.mucositis.doc + u.alt * i.alt.doc + u.ast * i.ast.doc + u.backpain * i.backpain.doc + u.cardiac * i.cardiac.doc + u.af * i.af.doc + u.hypok * i.hypok.doc)

c.tox.doc <- (c.neutropenia * i.neutropenia.doc + c.febneutropenia * i.febneutropenia.doc + c.neutropeniainfection * i.neutropeniainfection.doc + c.anaemia * i.anaemia.doc + c.thrombocytopenia * i.thrombocytopenia.doc + c.diarrhoea * i.diarrhoea.doc + c.sensneuro * i.sensneuro.doc + c.periphoedema * i.periphoedema.doc + c.dyspnoea * i.dyspnoea.doc + c.stomatitis * i.stomatitis.doc + c.mucositis * i.mucositis.doc + c.alt * i.alt.doc + c.ast * i.ast.doc + c.backpain * i.backpain.doc + c.cardiac * i.cardiac.doc + c.af * i.af.doc + c.hypok * i.hypok.doc)

u.tox.aa <- (u.neutropenia * i.neutropenia.aa + u.febneutropenia * i.febneutropenia.aa + u.neutropeniainfection * i.neutropeniainfection.aa + u.anaemia * i.anaemia.aa + u.thrombocytopenia * i.thrombocytopenia.aa + u.diarrhoea * i.diarrhoea.aa + u.sensneuro * i.sensneuro.aa + u.periphoedema * i.periphoedema.aa + u.dyspnoea * i.dyspnoea.aa + u.stomatitis * i.stomatitis.aa + u.mucositis * i.mucositis.aa + u.alt * i.alt.aa + u.ast * i.ast.aa + u.backpain * i.backpain.aa + u.cardiac * i.cardiac.aa + u.af * i.af.aa + u.hypok * i.hypok.aa)

c.tox.aa <- (c.neutropenia * i.neutropenia.aa + c.febneutropenia * i.febneutropenia.aa + c.neutropeniainfection * i.neutropeniainfection.aa + c.anaemia * i.anaemia.aa + c.thrombocytopenia * i.thrombocytopenia.aa + c.diarrhoea * i.diarrhoea.aa + c.sensneuro * i.sensneuro.aa + c.periphoedema * i.periphoedema.aa + c.dyspnoea * i.dyspnoea.aa + c.stomatitis * i.stomatitis.aa + c.mucositis * i.mucositis.aa + c.alt * i.alt.aa + c.ast * i.ast.aa + c.backpain * i.backpain.aa + c.cardiac * i.cardiac.aa + c.af * i.af.aa + c.hypok * i.hypok.aa)

####### INITIALIZATION ##########################################

m.TR <- matrix(NA, nrow = n.t + 1, ncol = n.s, 
               dimnames = list(0:n.t, v.n))        # create Markov trace

init <- c(1, 0, 0, 0)
m.TR[1, ] <- init

#define parameters for each strategy
params.ctl <- c(n.s = n.s,  
                n.t = n.t,  
                age = age, 
                hr.tx = 1,
                hr.crpc.tx = 1,
                p.prog = 0.05752261,
                p.pca.death = 0.02337927)

params.doc <- c(n.s = n.s,  
                n.t = n.t,  
                age = age, 
                hr.tx = 0.63,
                hr.crpc.tx = 1,
                p.prog = 0.05752261,
                p.pca.death = 0.02337927)

params.aa <- c(n.s = n.s,  
               n.t = n.t,  
               age = age, 
               hr.tx = 0.29,
               hr.crpc.tx = 1,
               p.prog = 0.05752261,
               p.pca.death = 0.02337927)

#model
model <- function(params){
  with(
    as.list(params),
    {
      r.all.death <- rep(life.table[age:nrow(life.table), 2], each = 12) 
      p.all.death <- 1-exp(-r.all.death[1:n.t])
      a.P <- array(0, dim = c(n.s, n.s, n.t), 
                   dimnames = list(v.n, v.n, 0:199))
      incArray <- array(0, dim = c(n.s, n.s, n.t + 1), 
                        dimnames = list(v.n, v.n, paste("Cycle", 
                                                        0:(n.t), sep = "")))
      
      init <- c(1, 0, 0, 0)
      m.TR[1, ] <- init
      diag(incArray[, , 1]) <- init
      
      ## From HSPC
      a.P["hspc", "hspc", ]           <- 1 - (p.prog * hr.tx + p.all.death)
      a.P["hspc", "crpc", ]           <- p.prog * hr.tx
      a.P["hspc", "all.death", ]      <- p.all.death
      ## From CRPC
      a.P["crpc", "crpc", ]           <- 1 - (hr.crpc.tx * p.pca.death + p.all.death) 
      a.P["crpc", "pca.death", ]      <- hr.crpc.tx * p.pca.death
      a.P["crpc", "all.death", ]      <- p.all.death
      ## Dead states
      a.P["pca.death", "pca.death", ] <- 1
      a.P["all.death", "all.death", ] <- 1
      
      for (t in 1:n.t){  
        m.TR[t + 1, ] <- m.TR[t, ] %*% a.P[,,t]
        incArray[, , t + 1] <- m.TR[t, ] * a.P[, , t]
      }
      trans.pcadeath <- incArray[c("crpc"), # From States
                                 c("pca.death"), # To States
                                 -1]
      results <- cbind(m.TR, trans.pcadeath)
      return(results)
    }
  )
}

#control output calculations
out.ctl <- model(params.ctl)
u.out.ctl <- out.ctl %*% c(u.hspc, u.crpc, 0, 0, 0)
d.u.ctl <- t(u.out.ctl) %*% v.dw
d.ly.ctl <- (t(out.ctl %*% c(1, 1, 0, 0, 0)) %*% v.dw)/12
d.c.ctl <- t(out.ctl %*% c(c.hspc, c.crpc, 0, 0, c.pca.death)) %*% v.dw
results.ctl <- c(d.c.ctl, d.u.ctl, d.ly.ctl)

# docetaxel output calculations
out.doc <- model(params.doc)
u.out.doc <- rbind(out.doc[0:6,] %*% c(u.doc-u.tox.doc, u.crpc, 0, 0, 0), out.doc[6:n.t+1,] %*% c(u.hspc, u.crpc, 0, 0, 0))
d.u.doc <- t(u.out.doc) %*% v.dw
d.ly.doc <- (t(out.doc %*% c(1, 1, 0, 0, 0)) %*% v.dw)/12
d.c.doc <- t(rbind(out.doc[0:6,] %*% c(c.doc + c.tox.doc + c.doc.adm + c.hspc, c.crpc, 0, 0, c.pca.death), out.doc[6:n.t+1,] %*% c(c.hspc, c.crpc, 0, 0, c.pca.death))) %*% v.dw
results.doc <- c(d.c.doc, d.u.doc, d.ly.doc)

# abiraterone output calculations
out.aa <- model(params.aa)
u.out.aa <- out.aa %*% c(u.aa-u.tox.aa, u.crpc, 0, 0, 0)
d.u.aa <- t(u.out.aa) %*% v.dw
d.ly.aa <- (t(out.aa %*% c(1, 1, 0, 0, 0)) %*% v.dw)/12
d.c.aa <- t(out.aa %*% c(c.aa + c.tox.aa + c.hspc, c.crpc, 0, 0, c.pca.death)) %*% v.dw
results.aa <- c(d.c.aa, d.u.aa, d.ly.aa)

#complete model output calculations
results.df <- rbind(results.ctl, results.doc, results.aa)
incr.cost <- c(NA, results.df[2,1] - results.df[1,1], results.df[3,1] - results.df[2,1])
incr.eff <- c(NA, results.df[2,2] - results.df[1,2], results.df[3,2] - results.df[2,2])
icer <- incr.cost / incr.eff
results.df <- cbind(results.df, incr.cost, incr.eff, icer)
colnames(results.df) <- c("Cost", "QALY", "LY", "Incr Cost", "Incr Eff", "ICER")
row.names(results.df) <- c("Control", "Docetaxel", "Abiraterone")
print(results.df)
