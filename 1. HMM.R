setwd('/Users/mac/Desktop/HMM')
stockTR = read.csv(file = 'training.csv') # read a file from the working directory
attach(stockTR)
set.seed(1)

#find state number, one kind of ovbservation. 5 states is optimal choice
hmm2=depmixS4::depmix(AMClose~1,data=stockTR, nstates=2, family=gaussian())
hm2=depmixS4::fit(hmm2, version=FALSE)
print(hm2)


hmm3=depmixS4::depmix(AMClose~1,data=stockTR, nstates=3, family=gaussian())
hm3=depmixS4::fit(hmm3)
print(hm3)


hmm4=depmixS4::depmix(AMClose~1,data=stockTR, nstates=4, family=gaussian())
hm4=depmixS4::fit(hmm4)
print(hm4)


hmm5=depmixS4::depmix(AMClose~1, data=stockTR,nstates=5,family=gaussian(),instart = runif(5))
hm5=depmixS4::fit(hmm5)
print(hm5)# best
summary(hm5)
forward=depmixS4::forwardbackward(hm5)
forward

hmm6=depmixS4::depmix(AMClose~1,data=stockTR, nstates=6, family=gaussian(link = 'identity'))
hm6=depmixS4::fit(hmm6)

##plot,  state5 1 variable
probs5 <- depmixS4::viterbi(hm5)[1]
prob55= as.numeric(unlist(probs5))
plot(AMClose, type = 'l', ylab = 'price&state(training 1 5)', )
par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
par(new = TRUE)
plot( prob55, type = "p", axes = FALSE,cex=0.2,ylab = '' , bty = "n" )
#matplot(probs[,-1], type='l', main='Probabilities', ylab='Probability')


###multi variable 4
#select model
hmm4=depmixS4::depmix(c(AMClose~1,AMOpen~1,AMLow~1,AMHigh~1), data=stockTR,nstates=5,family=list(gaussian(),gaussian(),gaussian(),gaussian()),instart = runif(5))
hm4=depmixS4::fit(hmm4)
print(hm4)


hmm44=depmixS4::depmix(c(AMClose~1,AMOpen~1,AMLow~1,AMHigh~1), data=stockTR,nstates=4,family=list(gaussian(),gaussian(),gaussian(),gaussian()),instart = runif(4))
hm44=depmixS4::fit(hmm44)
print(hm44)

hmm43=depmixS4::depmix(c(AMClose~1,AMOpen~1,AMLow~1,AMHigh~1), data=stockTR,nstates=3,family=list(gaussian(),gaussian(),gaussian(),gaussian()),instart = runif(3))
hm43=depmixS4::fit(hmm43)
print(hm43)

hmm42=depmixS4::depmix(c(AMClose~1,AMOpen~1,AMLow~1,AMHigh~1), data=stockTR,nstates=2,family=list(gaussian(),gaussian(),gaussian(),gaussian()),instart = runif(2))
hm42=depmixS4::fit(hmm42)
print(hm42)

hmm46=depmixS4::depmix(c(AMClose~1,AMOpen~1,AMLow~1,AMHigh~1), data=stockTR,nstates=6,family=list(gaussian(),gaussian(),gaussian(),gaussian()),instart = runif(6))
hm46=depmixS4::fit(hmm46)
print(hm46) # 6 state is the best one
summary(hm46)
forward=depmixS4::forwardbackward(hm46)
forward


## plot variable 4
probs46 <- depmixS4::viterbi(hm46)
prob46=probs46[1]
prob466= as.numeric(unlist(prob46))
plot(AMClose, type = 'l',ylab='price&state traning 4 6')
par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
par(new = TRUE)
plot(prob466, type = "p", axes = FALSE, bty = "n" ,cex=0.2,ylab = '')
#matplot(probs46[,-1], type='l', main='Probabilities', ylab='Probability')

###multi variable 9
#select model
hmm9=depmixS4::depmix(c(AMClose~1,AMOpen~1,AMLow~1,AMHigh~1,AMVolume~1,SPClose~1,SPHigh~1,SPLow~1,SPVolume~1), data=stockTR,nstates=5,family=list(gaussian(),gaussian(),gaussian(),gaussian(),gaussian(),gaussian(),gaussian(),gaussian(),gaussian()),instart = runif(5))
hm9=depmixS4::fit(hmm9)
print(hm9)    # 5 state is best 
summary(hm9)
forward=depmixS4::forwardbackward(hm9)
forward

hmm94=depmixS4::depmix(c(AMClose~1,AMOpen~1,AMLow~1,AMHigh~1,AMVolume~1,SPClose~1,SPHigh~1,SPLow~1,SPVolume~1), data=stockTR,nstates=4,family=list(gaussian(),gaussian(),gaussian(),gaussian(),gaussian(),gaussian(),gaussian(),gaussian(),gaussian()),instart = runif(4))
hm94=depmixS4::fit(hmm94)
print(hm94)
summary(hm4)

hmm93=depmixS4::depmix(c(AMClose~1,AMOpen~1,AMLow~1,AMHigh~1,AMVolume~1,SPClose~1,SPHigh~1,SPLow~1,SPVolume~1), data=stockTR,nstates=3,family=list(gaussian(),gaussian(),gaussian(),gaussian(),gaussian(),gaussian(),gaussian(),gaussian(),gaussian()),instart = runif(3))
hm93=depmixS4::fit(hmm93)
print(hm93)
summary(hm4)

hmm92=depmixS4::depmix(c(AMClose~1,AMOpen~1,AMLow~1,AMHigh~1,AMVolume~1,SPClose~1,SPHigh~1,SPLow~1,SPVolume~1), data=stockTR,nstates=2,family=list(gaussian(),gaussian(),gaussian(),gaussian(),gaussian(),gaussian(),gaussian(),gaussian(),gaussian()),instart = runif(2))
hm92=depmixS4::fit(hmm92)
print(hm92)

hmm96=depmixS4::depmix(c(AMClose~1,AMOpen~1,AMLow~1,AMHigh~1,AMVolume~1,SPClose~1,SPHigh~1,SPLow~1,SPVolume~1), data=stockTR,nstates=6,family=list(gaussian(),gaussian(),gaussian(),gaussian(),gaussian(),gaussian(),gaussian(),gaussian(),gaussian()),instart = runif(6))
hm96=depmixS4::fit(hmm96)
print(hm96)

##plot variavble 9, 5 state
probs9 <- depmixS4::viterbi(hm9)
prob9=probs9[1]
prob99= as.numeric(unlist(prob9))
plot(AMClose, type = 'l', ylab = 'price&state training 9 5')
par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
par(new = TRUE)
plot(prob99, type = "p", axes = FALSE, bty = "n",cex=0.2,ylab = '' )
#matplot(probs9[,-1], type='l', main='Probabilities', ylab='Probability')





####test model

### test one observation with 5 state
setwd('/Users/mac/Desktop/HMM')
stockTE = read.csv(file = 'test96.csv') # read a file from the working directory
attach(stockTE)
set.seed(1)
hmmT5=depmixS4::depmix(AMClose~1, data=stockTE,nstates=5,family=gaussian(),instart = runif(5))
hmT5=depmixS4::fit(hmmT5)
print(hmT5)
modNew1 <- depmixS4::setpars(hmT5, getpars(hm5),data=stockTE)
modNewfit1=fit(modNew1)
summary(modNewfit1)
##plot one observation with 5 state
probsmodNewfit1 <- depmixS4::viterbi(modNewfit1)
probmodNewfit1=probsmodNewfit1[1]
probmodNewfit11= as.numeric(unlist(probmodNewfit1))
plot(AMClose, type = 'l', ylab = 'close price&state test 1 5' )
par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
par(new = TRUE)
plot(probmodNewfit11, type = "p",cex = .2, ylab = '',axes = FALSE, bty = "n")
forward=depmixS4::forwardbackward(modNewfit1)
forward

######?This one-liner gets the probability of new data by running the forward algorithm on your original model.
#sum(forwardbackward(setpars(depmix(list(AMClose~1), data=stockTE, nstates=5,family=list(gaussian())), getpars(hm5)))[["alpha"]][nrow(data),])

### test 4 observation with 6 state
hmmT46=depmixS4::depmix(c(AMClose~1,AMOpen~1,AMLow~1,AMHigh~1), data=stockTE,nstates=6,family=list(gaussian(),gaussian(),gaussian(),gaussian()),instart = runif(6))
hmT46=depmixS4::fit(hmmT46)
print(hmT46)
modNewT46 <- depmixS4::setpars(hmT46, getpars(hm46),data=stockTE)
modNewfitT46=fit(modNewT46)
summary(modNewfitT46)
probsmodNewfitT46 <- depmixS4::viterbi(modNewfitT46)
probmodNewfitT46=probsmodNewfitT46[1]
probmodNewfitT466= as.numeric(unlist(probmodNewfitT46))
plot(AMClose, type = 'l', ylab = 'close price&state test 4 6' )
par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
par(new = TRUE)
plot(probmodNewfitT466, type = "p", axes = FALSE, bty = "n",cex = .2, ylab = '')
forward=depmixS4::forwardbackward(modNewfitT46)
forward

#a=depmixS4::forwardbackward(modNewfitT46)
#all.equal(-sum(log(a$sca)),a$logLike)
#sum(a$beta)

### test 9 observation with 6 state
hmmT9=depmixS4::depmix(c(AMClose~1,AMOpen~1,AMLow~1,AMHigh~1,AMVolume~1,SPClose~1,SPHigh~1,SPLow~1,SPVolume~1), data=stockTE,nstates=5,family=list(gaussian(),gaussian(),gaussian(),gaussian(),gaussian(),gaussian(),gaussian(),gaussian(),gaussian()),instart = runif(5))
hmT9=depmixS4::fit(hmmT9)
print(hmT9)    # 5 state is best 
summary(hmT9)
modNewT9 <- depmixS4::setpars(hmT9, getpars(hm9),data=stockTE)
modNewfitT9=fit(modNewT9)
summary(modNewfitT9)
probsmodNewfitT9 <- depmixS4::viterbi(modNewfitT9)
probmodNewfitT9=probsmodNewfitT9[1]
#b=replace(probmodNewfitT9, probmodNewfitT9==1, 0)

probmodNewfitT99= as.numeric(unlist(probmodNewfitT9))
plot(AMClose, type = 'l', ylab = 'close price&state 9 6' )
#par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
par(new = TRUE)
plot(probmodNewfitT9$state, type = "p", axes = FALSE, bty = "n",cex = .2, ylab = '')
forward=depmixS4::forwardbackward(modNewfitT9)
forward

#a=forwardbackward(modNewfitT9)

################# state sequence for each model(training and test)
probs5$state
probs46$state
probs9$state
probsmodNewfit1$state
probsmodNewfitT46$state
probsmodNewfitT9$state
### use test set calculate accumulative profit over 96 days

### 1 observation sequence, 5 states ,action sequence
b=replace(probmodNewfit1, probmodNewfit1==1, 0)
b=replace(b, b==2, 0)
b=replace(b, b==3, 1)
b=replace(b, b==4, 1)
b=replace(b, b==5, 1)
b
trading_price=b*stockTE$AMClose
b=trading_price[trading_price!='0']
sum(diff(b))#1.61

### 4 observation sequence, 6 states,action sequence
d=replace(probsmodNewfitT46$state, probsmodNewfitT46$state==1, 0)
d=replace(d, d==2, 0)
d=replace(d, d==3, 1)
d=replace(d, d==4, 1)
d=replace(d, d==5, 1)
d=replace(d, d==6, 1)
d
trading_price=d*stockTE$AMClose
d=trading_price[trading_price!='0']
sum(diff(d))#12.57
### 9 observation sequence, 5 states,action sequence
c=replace(probsmodNewfitT9$state, probsmodNewfitT9$state==1, 1)
c=replace(c, c==2, 0)
c=replace(c, c==3, 1)
c=replace(c, c==4, 1)
c=replace(c, c==5, 0)
c
trading_price=c*stockTE$AMClose
c=trading_price[trading_price!='0']
sum(diff(c))#5.75
h/


