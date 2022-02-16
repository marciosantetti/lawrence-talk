library(ggplot2)
library(gridExtra)
library(stevemisc)
library(ggthemr)
library(scales)
library(hexView)




vars <- readEViews('annual_data2.wf1')
vars$period <- seq(1949, 2018, 1)


ggthemr('greyscale')

first <- read.csv2('ln_x.csv', header=TRUE, sep=',', dec='.')
second <- read.csv2('ln_e.csv', header=TRUE, sep=',', dec='.')
third <- read.csv2('ln_r.csv', header=TRUE, sep=',', dec='.')
fourth <- read.csv2('ln_n.csv', header=TRUE, sep=',', dec='.')
fifth <-   read.csv2('ln_y.csv', header=TRUE, sep=',', dec='.')



####################################################





d.period <- vars$period[-1]

d.vars <- cbind(vars$DLN_B[-1], vars$DLN_E[-1], vars$DLN_R[-1], vars$DLN_X[-1], vars$DLN_Y[-1], 
                vars$B_CYCLE[-1], vars$E_CYCLE[-1], vars$R_CYCLE[-1], vars$X_CYCLE[-1], vars$Y_CYCLE[-1],
                vars$DLN_N[-1], vars$N_CYCLE[-1])

d.data <- data.frame(cbind(d.period, d.vars))

colnames(d.data) <- c('period', 'DLN_B', 'DLN_E', 'DLN_R', 'DLN_X', 'DLN_Y', 'B_CYCLE', 'E_CYCLE',
                      'R_CYCLE', 'X_CYCLE', 'Y_CYCLE', 'DLN_N', 'N_CYCLE')

#

ks.period <- vars$period[-c(1,70)]

ks.new <- vars$LN_KS[-c(1,70)]

ks.data <- data.frame(cbind(ks.period, ks.new))

colnames(ks.data) <- c('period', 'LN_KS')

#

ks.period2 <- vars$period[-c(1,2,70)]

ks.new2 <- cbind(vars$DLN_KS[-c(1,2,70)], vars$KS_CYCLE[-c(1,2,70)])

ks.data2 <- data.frame(cbind(ks.period2, ks.new2))

colnames(ks.data2) <- c('period', 'DLN_KS', 'KS_CYCLE')

#

tfp.period2 <- vars$period[-c(1,2,3,4,5,6,70)]

tfp.new2 <- cbind(vars$DLN_TFP[-c(1,2,3,4,5,6,70)], vars$TFP_CYCLE[-c(1,2,3,4,5,6,70)])

tfp.data2 <- data.frame(cbind(tfp.period2, tfp.new2))

colnames(tfp.data2) <- c('period', 'DLN_TFP', 'TFP_CYCLE')

# 

tfp.period <- vars$period[-c(1,2,3,4,5,70)]

tfp.new <- vars$TFP[-c(1,2,3,4,5,70)]

tfp.data <- data.frame(cbind(tfp.period, tfp.new))

colnames(tfp.data) <- c('period', 'TFP')


#####################



## FIG 1



ts1 <- ggplot(vars, aes(y=LN_Y, x=period)) + geom_line(size=1)  + 
  scale_x_continuous(breaks=c(1949, 1960, 1970, 1980, 1990, 2000, 2010, 2018)) +
  labs(title='Real GDP', x="", y="") + theme(axis.text= element_text(size = 13)) 

ts2 <- ggplot(vars, aes(y=LN_N, x=period)) + geom_line(size=1)  + 
  scale_x_continuous(breaks=c(1949, 1960, 1970, 1980, 1990, 2000, 2010, 2018)) +
  labs(title='Employment-to-population ratio', x="", y="") + theme(axis.text= element_text(size = 13)) 



ts1.1 <- ggplot(d.data, aes(x=period, y=Y_CYCLE)) + geom_line(col='grey3', alpha=0.9)  + 
  scale_x_continuous(breaks=c(1950, 1960, 1970, 1980, 1990, 2000, 2010, 2018)) +
  scale_y_continuous(limits=c(-6, 10)) +
  geom_line(aes(y=DLN_Y), col='grey37', alpha=0.5) +
  geom_hline(yintercept = 0) +
  labs(title='', x="", y="") + theme(axis.text= element_text(size = 13)) 

ts2.1 <- ggplot(d.data, aes(x=period, y=N_CYCLE)) + geom_line(col='grey3', alpha=0.9)  + 
  scale_x_continuous(breaks=c(1950, 1960, 1970, 1980, 1990, 2000, 2010, 2018)) +
  scale_y_continuous(limits=c(-5, 4)) +
  geom_line(aes(y=DLN_N), col='grey37', alpha=0.5) +
  geom_hline(yintercept = 0) +
  labs(title='', x="", y="") + theme(axis.text= element_text(size = 13)) 

multiplot(ts1, ts2, ts1.1, ts2.1, cols=2)



## FIG 2:

ts3 <- ggplot(vars, aes(y=LN_B, x=period)) + geom_line(size=1)  + 
  scale_x_continuous(breaks=c(1949, 1960, 1970, 1980, 1990, 2000, 2010, 2018)) +
  labs(title='Carbon dioxide emissions', x="", y="") + theme(axis.text= element_text(size = 13)) 

ts4 <- ggplot(vars, aes(y=LN_E, x=period)) + geom_line(size=1)  + 
  scale_x_continuous(breaks=c(1949, 1960, 1970, 1980, 1990, 2000, 2010, 2018)) +
  labs(title='Non-renewable energy use', x="", y="") + theme(axis.text= element_text(size = 13)) 

ts5 <- ggplot(vars, aes(y=LN_R, x=period)) + geom_line(size=1)  + 
  scale_x_continuous(breaks=c(1949, 1960, 1970, 1980, 1990, 2000, 2010, 2018)) +
  labs(title='Renewable energy use', x="", y="") + theme(axis.text= element_text(size = 13)) 

ts3.1 <- ggplot(d.data, aes(x=period, y=B_CYCLE)) + geom_line(col='grey3', alpha=0.9)  + 
  scale_x_continuous(breaks=c(1950, 1960, 1970, 1980, 1990, 2000, 2010, 2018)) +
  scale_y_continuous(limits=c(-6, 10)) +
  geom_line(aes(y=DLN_B), col='grey37', alpha=0.5) +
  geom_hline(yintercept = 0) +
  labs(title='', x="", y="") + theme(axis.text= element_text(size = 13)) 

ts4.1 <- ggplot(d.data, aes(x=period, y=E_CYCLE)) + geom_line(col='grey3', alpha=0.9)  + 
  scale_x_continuous(breaks=c(1950, 1960, 1970, 1980, 1990, 2000, 2010, 2018)) +
  scale_y_continuous(limits=c(-8, 12)) +
  geom_line(aes(y=DLN_E), col='grey37', alpha=0.7) +
  geom_hline(yintercept = 0) +
  labs(title='', x="", y="") + theme(axis.text= element_text(size = 13)) 

ts5.1 <- ggplot(d.data, aes(x=period, y=R_CYCLE)) + geom_line(col='grey3', alpha=0.9)  + 
  scale_x_continuous(breaks=c(1950, 1960, 1970, 1980, 1990, 2000, 2010, 2018)) +
  scale_y_continuous(limits=c(-20, 20)) +
  geom_line(aes(y=DLN_R), col='grey37', alpha=0.7) +
  geom_hline(yintercept = 0) +
  labs(title='', x="", y="") + theme(axis.text= element_text(size = 13)) 


multiplot(ts3, ts4, ts5, ts3.1, ts4.1, ts5.1, cols=2)


###### FIG 3:



ts6 <- ggplot(vars, aes(y=LN_X, x=period)) + geom_line(size=1)  + 
  scale_x_continuous(breaks=c(1949, 1960, 1970, 1980, 1990, 2000, 2010, 2018)) +
  labs(title='Labor productivity', x="", y="") + theme(axis.text= element_text(size = 13)) 

ts7 <- ggplot(tfp.data, aes(y=TFP, x=period)) + geom_line(size=1)  + 
  scale_x_continuous(breaks=c(1950, 1960, 1970, 1980, 1990, 2000, 2010, 2018)) +
  labs(title='Total factor productivity', x="", y="") + theme(axis.text= element_text(size = 13)) 

ts8 <- ggplot(ks.data, aes(y=LN_KS, x=period)) + geom_line(size=1)  + 
  scale_x_continuous(breaks=c(1950, 1960, 1970, 1980, 1990, 2000, 2010, 2018)) +
  labs(title='Aggregate capital stock', x="", y="") + theme(axis.text= element_text(size = 13)) 

ts6.1 <- ggplot(d.data, aes(x=period, y=X_CYCLE)) + geom_line(col='grey3', alpha=0.9)  + 
  scale_x_continuous(breaks=c(1950, 1960, 1970, 1980, 1990, 2000, 2010, 2018)) +
  scale_y_continuous(limits=c(-4, 7)) +
  geom_line(aes(y=DLN_X), col='grey37', alpha=0.7) +
  geom_hline(yintercept = 0) +
  labs(title='', x="", y="") + theme(axis.text= element_text(size = 13)) 





ts8.1 <- ggplot(ks.data2, aes(x=period, y=KS_CYCLE)) + geom_line(col='grey3', alpha=0.9)  + 
  scale_x_continuous(breaks=c(1950, 1960, 1970, 1980, 1990, 2000, 2010, 2018)) +
  scale_y_continuous(limits=c(-2, 5)) +
  geom_line(aes(y=DLN_KS), col='grey37', alpha=0.7) +
  geom_hline(yintercept = 0) +
  labs(title='', x="", y="") + theme(axis.text= element_text(size = 13)) 





ts7.1 <- ggplot(tfp.data2, aes(x=period, y=TFP_CYCLE)) + geom_line(col='grey3', alpha=0.9)  + 
  scale_x_continuous(breaks=c(1955, 1970, 1980, 1990, 2000, 2010, 2018)) +
  scale_y_continuous(limits=c(-4, 4)) +
  geom_line(aes(y=DLN_TFP), col='grey37', alpha=0.7) +
  geom_hline(yintercept = 0) +
  labs(title='', x="", y="") + theme(axis.text= element_text(size = 13)) 



multiplot(ts6, ts7, ts8, ts6.1, ts7.1, ts8.1, cols=2)


#### FIG 4:


ts9 <- ggplot(vars, aes(y=EY*100, x=period)) + geom_line(size=1)  + 
  scale_x_continuous(breaks=c(1949, 1960, 1970, 1980, 1990, 2000, 2010, 2018)) +
  labs(title='Energy intensity', x="", y='', subtitle=expression(10^-2)) + 
  theme(axis.text= element_text(size = 13)) 

ts10 <- ggplot(vars, aes(y=EL*1000, x=period)) + geom_line(size=1)  + 
  scale_x_continuous(breaks=c(1949, 1960, 1970, 1980, 1990, 2000, 2010, 2018)) +
  scale_y_continuous(breaks=pretty_breaks(n=5)) +
  labs(title='Energy-labor ratio', x="", y="", subtitle=expression(10^-3)) + 
  theme(axis.text= element_text(size = 13)) 

ts11 <- ggplot(vars, aes(y=BE, x=period)) + geom_line(size=1)  + 
  scale_x_continuous(breaks=c(1949, 1960, 1970, 1980, 1990, 2000, 2010, 2018)) +
  labs(title='Emissions intensity of energy use', x="", y="") + theme(axis.text= element_text(size = 13)) 

ts12 <- ggplot(vars, aes(y=BL*100, x=period)) + geom_line(size=1)  + 
  scale_x_continuous(breaks=c(1949, 1960, 1970, 1980, 1990, 2000, 2010, 2018)) +
  labs(title='Emissions per worker', x="", y="", subtitle=expression(10^-2)) + 
  theme(axis.text= element_text(size = 13)) 



multiplot(ts9, ts10, ts11, ts12, cols=2)



##################################################



### FIG 5 (6x6 VAR)


one <- read.csv2('fvar_levels_row1.csv', header=TRUE, sep=',', dec='.')
two <- read.csv2('fvar_levels_row2.csv', header=TRUE, sep=',', dec='.')
three <- read.csv2('fvar_levels_row3.csv', header=TRUE, sep=',', dec='.')
four <- read.csv2('fvar_levels_row4.csv', header=TRUE, sep=',', dec='.')
five <- read.csv2('fvar_levels_row5.csv', header=TRUE, sep=',', dec='.')
six <- read.csv2('fvar_levels_row6.csv', header=TRUE, sep=',', dec='.')


## First column:

f11 <- ggplot(one, aes(x=period, y=ln_x)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se1), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se2), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(X[t] %->% X[t]))

f21 <- ggplot(two, aes(x=period, y=ln_x)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se1), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se2), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(X[t] %->% E[t]))

f31 <- ggplot(three, aes(x=period, y=ln_x)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se1), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se2), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(X[t] %->% R[t]))

f41 <- ggplot(four, aes(x=period, y=ln_x)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se1), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se2), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(X[t] %->% P[t]))

f51 <- ggplot(five, aes(x=period, y=ln_x)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se1), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se2), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(X[t] %->% Y[t]))

f61 <- ggplot(six, aes(x=period, y=ln_x)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se1), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se2), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(X[t] %->% Phi[t]))


## Second column:

f12 <- ggplot(one, aes(x=period, y=ln_e)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se3), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se4), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(E[t] %->% X[t]))

f22 <- ggplot(two, aes(x=period, y=ln_e)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se3), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se4), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(E[t] %->% E[t]))

f32 <- ggplot(three, aes(x=period, y=ln_e)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se3), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se4), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(E[t] %->% R[t]))

f42 <- ggplot(four, aes(x=period, y=ln_e)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se3), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se4), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(E[t] %->% P[t]))

f52 <- ggplot(five, aes(x=period, y=ln_e)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se3), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se4), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(E[t] %->% Y[t]))

f62 <- ggplot(six, aes(x=period, y=ln_e)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se3), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se4), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(E[t] %->% Phi[t]))

## Third column:


f13 <- ggplot(one, aes(x=period, y=ln_r)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se5), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se6), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(R[t] %->% X[t]))

f23 <- ggplot(two, aes(x=period, y=ln_r)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se5), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se6), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(R[t] %->% E[t]))

f33 <- ggplot(three, aes(x=period, y=ln_r)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se5), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se6), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(R[t] %->% R[t]))

f43 <- ggplot(four, aes(x=period, y=ln_r)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se5), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se6), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(R[t] %->% P[t]))

f53 <- ggplot(five, aes(x=period, y=ln_r)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se5), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se6), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(R[t] %->% Y[t]))

f63 <- ggplot(six, aes(x=period, y=ln_r)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se5), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se6), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(R[t] %->% Phi[t]))


# Fourth column:

f14 <- ggplot(one, aes(x=period, y=ln_n)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se7), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se8), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(P[t] %->% X[t]))

f24 <- ggplot(two, aes(x=period, y=ln_n)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se7), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se8), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(P[t] %->% E[t]))

f34 <- ggplot(three, aes(x=period, y=ln_n)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se7), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se8), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(P[t] %->% R[t]))

f44 <- ggplot(four, aes(x=period, y=ln_n)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se7), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se8), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(P[t] %->% P[t]))

f54 <- ggplot(five, aes(x=period, y=ln_n)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se7), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se8), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(P[t] %->% Y[t]))

f64 <- ggplot(six, aes(x=period, y=ln_n)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se7), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se8), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(P[t] %->% Phi[t]))



# Fifth column:

f15 <- ggplot(one, aes(x=period, y=ln_y)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se9), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se10), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(Y[t] %->% X[t]))

f25 <- ggplot(two, aes(x=period, y=ln_y)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se9), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se10), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(Y[t] %->% E[t]))

f35 <- ggplot(three, aes(x=period, y=ln_y)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se9), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se10), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(Y[t] %->% R[t]))

f45 <- ggplot(four, aes(x=period, y=ln_y)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se9), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se10), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(Y[t] %->% P[t]))

f55 <- ggplot(five, aes(x=period, y=ln_y)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se9), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se10), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(Y[t] %->% Y[t]))

f65 <- ggplot(six, aes(x=period, y=ln_y)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se9), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se10), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(Y[t] %->% Phi[t]))


## Sixth Column:

f16 <- ggplot(one, aes(x=period, y=ln_b)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se11), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se12), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(Phi[t] %->% X[t]))

f26 <- ggplot(two, aes(x=period, y=ln_b)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se11), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se12), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(Phi[t] %->% E[t]))

f36 <- ggplot(three, aes(x=period, y=ln_b)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se11), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se12), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(Phi[t] %->% R[t]))

f46 <- ggplot(four, aes(x=period, y=ln_b)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se11), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se12), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(Phi[t] %->% P[t]))

f56 <- ggplot(five, aes(x=period, y=ln_b)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se11), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se12), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(Phi[t] %->% Y[t]))

f66 <- ggplot(six, aes(x=period, y=ln_b)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se11), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se12), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(Phi[t] %->% Phi[t]))


multiplot(f11, f21, f31, f41, f51, f61, 
          f12, f22, f32, f42, f52, f62,
          f13, f23, f33, f43, f53, f63,
          f14, f24, f34, f44, f54, f64,
          f15, f25, f35, f45, f55, f65,
          f16, f26, f36, f46, f56, f66, cols=6)


##################################################

### FIG 6 (5x5 VAR):


# First column:

f11 <- ggplot(first, aes(x=period, y=ln_x)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se1), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se2), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(X[t] %->% X[t]))


f21 <- ggplot(second, aes(x=period, y=ln_x)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se1), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se2), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(X[t] %->% E[t]))

f31 <- ggplot(third, aes(x=period, y=ln_x)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se1), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se2), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(X[t] %->% R[t]))

f41 <- ggplot(fourth, aes(x=period, y=ln_x)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se1), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se2), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(X[t] %->% P[t]))

f51 <- ggplot(fifth, aes(x=period, y=ln_x)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se1), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se2), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(X[t] %->% Y[t]))


# Second column:


f12 <- ggplot(first, aes(x=period, y=ln_e)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se3), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se4), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(E[t] %->% X[t]))

f22 <- ggplot(second, aes(x=period, y=ln_e)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se3), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se4), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(E[t] %->% E[t]))

f32 <- ggplot(third, aes(x=period, y=ln_e)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se3), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se4), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(E[t] %->% R[t]))

f42 <- ggplot(fourth, aes(x=period, y=ln_e)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se3), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se4), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(E[t] %->% P[t]))

f52 <- ggplot(fifth, aes(x=period, y=ln_e)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se3), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se4), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(E[t] %->% Y[t]))



# Third column:

f13 <- ggplot(first, aes(x=period, y=ln_r)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se5), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se6), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(R[t] %->% X[t]))

f23 <- ggplot(second, aes(x=period, y=ln_r)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se5), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se6), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(R[t] %->% E[t]))

f33 <- ggplot(third, aes(x=period, y=ln_r)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se5), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se6), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(R[t] %->% R[t]))

f43 <- ggplot(fourth, aes(x=period, y=ln_r)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se5), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se6), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(R[t] %->% P[t]))

f53 <- ggplot(fifth, aes(x=period, y=ln_r)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se5), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se6), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(R[t] %->% Y[t]))


# Fourth column:

f14 <- ggplot(first, aes(x=period, y=ln_n)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se7), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se8), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(P[t] %->% X[t]))

f24 <- ggplot(second, aes(x=period, y=ln_n)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se7), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se8), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(P[t] %->% E[t]))

f34 <- ggplot(third, aes(x=period, y=ln_n)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se7), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se8), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(P[t] %->% R[t]))

f44 <- ggplot(fourth, aes(x=period, y=ln_n)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se7), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se8), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(P[t] %->% P[t]))

f54 <- ggplot(fifth, aes(x=period, y=ln_n)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se7), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se8), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(P[t] %->% Y[t]))


# Fifth column:

f15 <- ggplot(first, aes(x=period, y=ln_y)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se9), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se10), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(Y[t] %->% X[t]))

f25 <- ggplot(second, aes(x=period, y=ln_y)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se9), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se10), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(Y[t] %->% E[t]))

f35 <- ggplot(third, aes(x=period, y=ln_y)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se9), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se10), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(Y[t] %->% R[t]))

f45 <- ggplot(fourth, aes(x=period, y=ln_y)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se9), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se10), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(Y[t] %->% P[t]))

f55 <- ggplot(fifth, aes(x=period, y=ln_y)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se9), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se10), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(Y[t] %->% Y[t]))



multiplot(f11, f21, f31, f41, f51, 
           f12, f22, f32, f42, f52, 
           f13, f23, f33, f43, f53,
           f14, f24, f34, f44, f54, 
           f15, f25, f35, f45, f55, cols=5)



### FIG 7 (Response to structural shocks):


s1 <- read.csv2('s1.csv', header=T, dec='.', sep=',')
s2 <- read.csv2('s2.csv', header=T, dec='.', sep=',')
s3 <- read.csv2('s3.csv', header=T, dec='.', sep=',')
s4 <- read.csv2('s4.csv', header=T, dec='.', sep=',')
s5 <- read.csv2('s5.csv', header=T, dec='.', sep=',')


f7.1 <- ggplot(s1, aes(x=period, y=ln_b)) + geom_line(lwd=1, col='grey45') +
  geom_line(aes(y=se1), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se2), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
  labs(x="", y="") + ggtitle('Technology shock') +
  theme(axis.text= element_text(size = 13))

f7.2 <- ggplot(s2, aes(x=period, y=ln_b)) + geom_line(lwd=1, col='grey45') +
  geom_line(aes(y=se1), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se2), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
  labs(x="", y="") + ggtitle('Non-renewable energy use shock') +
  theme(axis.text= element_text(size = 13))

f7.3 <- ggplot(s3, aes(x=period, y=ln_b)) + geom_line(lwd=1, col='grey45') +
  geom_line(aes(y=se1), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se2), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
  labs(x="", y="") + ggtitle('Renewable energy use shock') +
  theme(axis.text= element_text(size = 13))

f7.4 <- ggplot(s4, aes(x=period, y=ln_b)) + geom_line(lwd=1, col='grey45') +
  geom_line(aes(y=se1), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se2), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
  labs(x="", y="") + ggtitle('Population shock') +
  theme(axis.text= element_text(size = 13))

f7.5 <- ggplot(s5, aes(x=period, y=ln_b)) + geom_line(lwd=1, col='grey45') +
  geom_line(aes(y=se1), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se2), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
  labs(x="", y="") + ggtitle('Output shock') +
  theme(axis.text= element_text(size = 13))
  
multiplot(f7.1, f7.4, f7.2, f7.5, f7.3, cols=3)



## FIG 9 (Robustness for second step):


robdiff1 <- read.csv2('s6-7.csv', header=T, dec='.', sep=',')
robdiff2 <- read.csv2('s11-12.csv', header=T, dec='.', sep=',')
robdiff3 <- read.csv2('s13-14.csv', header=T, dec='.', sep=',')



hp1 <- read.csv2('s15-16.csv', header=T, dec='.', sep=',')
hp2 <- read.csv2('s17-18.csv', header=T, dec='.', sep=',')
hp3 <- read.csv2('s19-20.csv', header=T, dec='.', sep=',')


fd_x <- ggplot(robdiff1, aes(x=period, y=ln_b)) + geom_line(col='grey37', alpha=0.7, lwd=1) +
  geom_line(aes(y=se1), linetype='dashed', col='grey37') +
  geom_line(aes(y=se2), linetype='dashed', col='grey37') +
  geom_line(aes(y=se1e), linetype='dotdash', col='grey3', lwd=1) +
  geom_line(aes(y=se2e), linetype='dotdash', col='grey3', lwd=1) +
  geom_hline(yintercept=0) +
  geom_line(aes(y=ln_be), col='grey3', alpha=0.7, lwd=1) +
  scale_x_continuous(breaks=c(2,4,6,8,10)) +
  labs(x="", y="", subtitle='First differences') + ggtitle('Labor productivity') +
  theme(axis.text= element_text(size = 13))

fd_k <- ggplot(robdiff2, aes(x=period, y=ln_b)) + geom_line(col='grey37', alpha=0.7, lwd=1) +
  geom_line(aes(y=se1), linetype='dashed', col='grey37') +
  geom_line(aes(y=se2), linetype='dashed', col='grey37') +
  geom_line(aes(y=se1e), linetype='dotdash', col='grey3', lwd=1) +
  geom_line(aes(y=se2e), linetype='dotdash', col='grey3', lwd=1) +
  geom_hline(yintercept=0) +
  geom_line(aes(y=ln_be), col='grey3', alpha=0.7, lwd=1) +
  scale_x_continuous(breaks=c(2,4,6,8,10)) +
  labs(x="", y="", subtitle='First differences') + ggtitle('Capital stock') +
  theme(axis.text= element_text(size = 13))

fd_tfp <- ggplot(robdiff3, aes(x=period, y=ln_b)) + geom_line(col='grey37', alpha=0.7, lwd=1) +
  geom_line(aes(y=se1), linetype='dashed', col='grey37') +
  geom_line(aes(y=se2), linetype='dashed', col='grey37') +
  geom_line(aes(y=se1e), linetype='dotdash', col='grey3', lwd=1) +
  geom_line(aes(y=se2e), linetype='dotdash', col='grey3', lwd=1) +
  geom_hline(yintercept=0) +
  geom_line(aes(y=ln_be), col='grey3', alpha=0.7, lwd=1) +
  scale_x_continuous(breaks=c(2,4,6,8,10)) +
  labs(x="", y="", subtitle='First differences') + ggtitle('Total factor productivity') +
  theme(axis.text= element_text(size = 13))


hp_x <- ggplot(hp1, aes(x=period, y=ln_b)) + geom_line(col='grey37', alpha=0.7, lwd=1) +
  geom_line(aes(y=se1), linetype='dashed', col='grey37') +
  geom_line(aes(y=se2), linetype='dashed', col='grey37') +
  geom_line(aes(y=se1e), linetype='dotdash', col='grey3', lwd=1) +
  geom_line(aes(y=se2e), linetype='dotdash', col='grey3', lwd=1) +
  geom_hline(yintercept=0) +
  geom_line(aes(y=ln_be), col='grey3', alpha=0.7, lwd=1) +
  scale_x_continuous(breaks=c(2,4,6,8,10)) +
  labs(x="", y="", subtitle='HP filter') + ggtitle('Labor productivity') +
  theme(axis.text= element_text(size = 13))

hp_k <- ggplot(hp2, aes(x=period, y=ln_b)) + geom_line(col='grey37', alpha=0.7, lwd=1) +
  geom_line(aes(y=se1), linetype='dashed', col='grey37') +
  geom_line(aes(y=se2), linetype='dashed', col='grey37') +
  geom_line(aes(y=se1e), linetype='dotdash', col='grey3', lwd=1) +
  geom_line(aes(y=se2e), linetype='dotdash', col='grey3', lwd=1) +
  geom_hline(yintercept=0) +
  geom_line(aes(y=ln_be), col='grey3', alpha=0.7, lwd=1) +
  scale_x_continuous(breaks=c(2,4,6,8,10)) +
  labs(x="", y="", subtitle='HP filter') + ggtitle('Capital stock') +
  theme(axis.text= element_text(size = 13))

hp_tfp <- ggplot(hp3, aes(x=period, y=ln_b)) + geom_line(col='grey37', alpha=0.7, lwd=1) +
  geom_line(aes(y=se1), linetype='dashed', col='grey37') +
  geom_line(aes(y=se2), linetype='dashed', col='grey37') +
  geom_line(aes(y=se1e), linetype='dotdash', col='grey3', lwd=1) +
  geom_line(aes(y=se2e), linetype='dotdash', col='grey3', lwd=1) +
  geom_hline(yintercept=0) +
  geom_line(aes(y=ln_be), col='grey3', alpha=0.7, lwd=1) +
  scale_x_continuous(breaks=c(2,4,6,8,10)) +
  labs(x="", y="", subtitle='HP filter') + ggtitle('Total factor productivity') +
  theme(axis.text= element_text(size = 13))

multiplot(fd_x, hp_x, fd_k, hp_k, fd_tfp, hp_tfp, cols=3)




### FIG 8.1 (robustness for one-step VAR, with diff_ks)


one <- read.csv2('fvar_diff_ks_row1.csv', header=TRUE, sep=',', dec='.')
two <- read.csv2('fvar_diff_ks_row2.csv', header=TRUE, sep=',', dec='.')
three <- read.csv2('fvar_diff_ks_row3.csv', header=TRUE, sep=',', dec='.')
four <- read.csv2('fvar_diff_ks_row4.csv', header=TRUE, sep=',', dec='.')
five <- read.csv2('fvar_diff_ks_row5.csv', header=TRUE, sep=',', dec='.')
six <- read.csv2('fvar_diff_ks_row6.csv', header=TRUE, sep=',', dec='.')


## First column:

f11 <- ggplot(one, aes(x=period, y=dln_ks)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se1), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se2), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(K[t] %->% K[t]))

f21 <- ggplot(two, aes(x=period, y=dln_ks)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se1), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se2), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(K[t] %->% E[t]))

f31 <- ggplot(three, aes(x=period, y=dln_ks)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se1), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se2), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(K[t] %->% R[t]))

f41 <- ggplot(four, aes(x=period, y=dln_ks)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se1), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se2), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(K[t] %->% P[t]))

f51 <- ggplot(five, aes(x=period, y=dln_ks)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se1), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se2), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(K[t] %->% Y[t]))

f61 <- ggplot(six, aes(x=period, y=dln_ks)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se1), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se2), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(K[t] %->% Phi[t]))


## Second column:

f12 <- ggplot(one, aes(x=period, y=dln_e)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se3), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se4), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(E[t] %->% K[t]))

f22 <- ggplot(two, aes(x=period, y=dln_e)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se3), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se4), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(E[t] %->% E[t]))

f32 <- ggplot(three, aes(x=period, y=dln_e)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se3), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se4), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(E[t] %->% R[t]))

f42 <- ggplot(four, aes(x=period, y=dln_e)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se3), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se4), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(E[t] %->% P[t]))

f52 <- ggplot(five, aes(x=period, y=dln_e)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se3), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se4), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(E[t] %->% Y[t]))

f62 <- ggplot(six, aes(x=period, y=dln_e)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se3), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se4), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(E[t] %->% Phi[t]))

## Third column:


f13 <- ggplot(one, aes(x=period, y=dln_r)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se5), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se6), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(R[t] %->% K[t]))

f23 <- ggplot(two, aes(x=period, y=dln_r)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se5), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se6), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(R[t] %->% E[t]))

f33 <- ggplot(three, aes(x=period, y=dln_r)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se5), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se6), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(R[t] %->% R[t]))

f43 <- ggplot(four, aes(x=period, y=dln_r)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se5), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se6), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(R[t] %->% P[t]))

f53 <- ggplot(five, aes(x=period, y=dln_r)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se5), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se6), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(R[t] %->% Y[t]))

f63 <- ggplot(six, aes(x=period, y=dln_r)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se5), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se6), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(R[t] %->% Phi[t]))


# Fourth column:

f14 <- ggplot(one, aes(x=period, y=dln_n)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se7), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se8), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(P[t] %->% K[t]))

f24 <- ggplot(two, aes(x=period, y=dln_n)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se7), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se8), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(P[t] %->% E[t]))

f34 <- ggplot(three, aes(x=period, y=dln_n)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se7), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se8), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(P[t] %->% R[t]))

f44 <- ggplot(four, aes(x=period, y=dln_n)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se7), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se8), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(P[t] %->% P[t]))

f54 <- ggplot(five, aes(x=period, y=dln_n)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se7), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se8), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(P[t] %->% Y[t]))

f64 <- ggplot(six, aes(x=period, y=dln_n)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se7), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se8), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(P[t] %->% Phi[t]))



# Fifth column:

f15 <- ggplot(one, aes(x=period, y=dln_y)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se9), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se10), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(Y[t] %->% K[t]))

f25 <- ggplot(two, aes(x=period, y=dln_y)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se9), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se10), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(Y[t] %->% E[t]))

f35 <- ggplot(three, aes(x=period, y=dln_y)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se9), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se10), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(Y[t] %->% R[t]))

f45 <- ggplot(four, aes(x=period, y=dln_y)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se9), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se10), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(Y[t] %->% P[t]))

f55 <- ggplot(five, aes(x=period, y=dln_y)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se9), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se10), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(Y[t] %->% Y[t]))

f65 <- ggplot(six, aes(x=period, y=dln_y)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se9), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se10), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(Y[t] %->% Phi[t]))


## Sixth Column:

f16 <- ggplot(one, aes(x=period, y=dln_b)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se11), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se12), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(Phi[t] %->% K[t]))

f26 <- ggplot(two, aes(x=period, y=dln_b)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se11), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se12), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(Phi[t] %->% E[t]))

f36 <- ggplot(three, aes(x=period, y=dln_b)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se11), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se12), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(Phi[t] %->% R[t]))

f46 <- ggplot(four, aes(x=period, y=dln_b)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se11), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se12), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(Phi[t] %->% P[t]))

f56 <- ggplot(five, aes(x=period, y=dln_b)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se11), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se12), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(Phi[t] %->% Y[t]))

f66 <- ggplot(six, aes(x=period, y=dln_b)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se11), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se12), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(Phi[t] %->% Phi[t]))


multiplot(f11, f21, f31, f41, f51, f61, 
          f12, f22, f32, f42, f52, f62,
          f13, f23, f33, f43, f53, f63,
          f14, f24, f34, f44, f54, f64,
          f15, f25, f35, f45, f55, f65,
          f16, f26, f36, f46, f56, f66, cols=6)

multiplot(f61 + theme(axis.text= element_text(size = 13), plot.title=element_text(size=18)), 
          f64 + theme(axis.text= element_text(size = 13), plot.title=element_text(size=18)),
          f62+ theme(axis.text= element_text(size = 13), plot.title=element_text(size=18)), 
          f65 + theme(axis.text= element_text(size = 13), plot.title=element_text(size=18)), 
          f63 + theme(axis.text= element_text(size = 13), plot.title=element_text(size=18)), 
          f66 + theme(axis.text= element_text(size = 13), plot.title=element_text(size=18)), cols=3)




#################################################


### FIG 8.2 (robustness for one-step VAR, with diff_x)


one <- read.csv2('fvar_diff_row1.csv', header=TRUE, sep=',', dec='.')
two <- read.csv2('fvar_diff_row2.csv', header=TRUE, sep=',', dec='.')
three <- read.csv2('fvar_diff_row3.csv', header=TRUE, sep=',', dec='.')
four <- read.csv2('fvar_diff_row4.csv', header=TRUE, sep=',', dec='.')
five <- read.csv2('fvar_diff_row5.csv', header=TRUE, sep=',', dec='.')
six <- read.csv2('fvar_diff_row6.csv', header=TRUE, sep=',', dec='.')


## First column:

f11 <- ggplot(one, aes(x=period, y=dln_x)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se1), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se2), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(X[t] %->% X[t]))

f21 <- ggplot(two, aes(x=period, y=dln_x)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se1), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se2), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(X[t] %->% E[t]))

f31 <- ggplot(three, aes(x=period, y=dln_x)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se1), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se2), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(X[t] %->% R[t]))

f41 <- ggplot(four, aes(x=period, y=dln_x)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se1), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se2), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(X[t] %->% P[t]))

f51 <- ggplot(five, aes(x=period, y=dln_x)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se1), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se2), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(X[t] %->% Y[t]))

f61 <- ggplot(six, aes(x=period, y=dln_x)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se1), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se2), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(X[t] %->% Phi[t]))


## Second column:

f12 <- ggplot(one, aes(x=period, y=dln_e)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se3), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se4), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(E[t] %->% X[t]))

f22 <- ggplot(two, aes(x=period, y=dln_e)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se3), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se4), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(E[t] %->% E[t]))

f32 <- ggplot(three, aes(x=period, y=dln_e)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se3), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se4), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(E[t] %->% R[t]))

f42 <- ggplot(four, aes(x=period, y=dln_e)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se3), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se4), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(E[t] %->% P[t]))

f52 <- ggplot(five, aes(x=period, y=dln_e)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se3), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se4), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(E[t] %->% Y[t]))

f62 <- ggplot(six, aes(x=period, y=dln_e)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se3), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se4), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(E[t] %->% Phi[t]))

## Third column:


f13 <- ggplot(one, aes(x=period, y=dln_r)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se5), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se6), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(R[t] %->% X[t]))

f23 <- ggplot(two, aes(x=period, y=dln_r)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se5), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se6), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(R[t] %->% E[t]))

f33 <- ggplot(three, aes(x=period, y=dln_r)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se5), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se6), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(R[t] %->% R[t]))

f43 <- ggplot(four, aes(x=period, y=dln_r)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se5), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se6), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(R[t] %->% P[t]))

f53 <- ggplot(five, aes(x=period, y=dln_r)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se5), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se6), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(R[t] %->% Y[t]))

f63 <- ggplot(six, aes(x=period, y=dln_r)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se5), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se6), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(R[t] %->% Phi[t]))


# Fourth column:

f14 <- ggplot(one, aes(x=period, y=dln_n)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se7), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se8), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(P[t] %->% X[t]))

f24 <- ggplot(two, aes(x=period, y=dln_n)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se7), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se8), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(P[t] %->% E[t]))

f34 <- ggplot(three, aes(x=period, y=dln_n)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se7), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se8), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(P[t] %->% R[t]))

f44 <- ggplot(four, aes(x=period, y=dln_n)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se7), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se8), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(P[t] %->% P[t]))

f54 <- ggplot(five, aes(x=period, y=dln_n)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se7), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se8), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(P[t] %->% Y[t]))

f64 <- ggplot(six, aes(x=period, y=dln_n)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se7), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se8), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(P[t] %->% Phi[t]))



# Fifth column:

f15 <- ggplot(one, aes(x=period, y=dln_y)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se9), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se10), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(Y[t] %->% X[t]))

f25 <- ggplot(two, aes(x=period, y=dln_y)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se9), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se10), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(Y[t] %->% E[t]))

f35 <- ggplot(three, aes(x=period, y=dln_y)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se9), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se10), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(Y[t] %->% R[t]))

f45 <- ggplot(four, aes(x=period, y=dln_y)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se9), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se10), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(Y[t] %->% P[t]))

f55 <- ggplot(five, aes(x=period, y=dln_y)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se9), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se10), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(Y[t] %->% Y[t]))

f65 <- ggplot(six, aes(x=period, y=dln_y)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se9), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se10), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(Y[t] %->% Phi[t]))


## Sixth Column:

f16 <- ggplot(one, aes(x=period, y=dln_b)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se11), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se12), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(Phi[t] %->% X[t]))

f26 <- ggplot(two, aes(x=period, y=dln_b)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se11), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se12), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(Phi[t] %->% E[t]))

f36 <- ggplot(three, aes(x=period, y=dln_b)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se11), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se12), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(Phi[t] %->% R[t]))

f46 <- ggplot(four, aes(x=period, y=dln_b)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se11), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se12), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(Phi[t] %->% P[t]))

f56 <- ggplot(five, aes(x=period, y=dln_b)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se11), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se12), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(Phi[t] %->% Y[t]))

f66 <- ggplot(six, aes(x=period, y=dln_b)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se11), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se12), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(Phi[t] %->% Phi[t]))


multiplot(f11, f21, f31, f41, f51, f61, 
          f12, f22, f32, f42, f52, f62,
          f13, f23, f33, f43, f53, f63,
          f14, f24, f34, f44, f54, f64,
          f15, f25, f35, f45, f55, f65,
          f16, f26, f36, f46, f56, f66, cols=6)


multiplot(f61 + theme(axis.text= element_text(size = 13), plot.title=element_text(size=18)), 
          f64 + theme(axis.text= element_text(size = 13), plot.title=element_text(size=18)),
          f62+ theme(axis.text= element_text(size = 13), plot.title=element_text(size=18)), 
          f65 + theme(axis.text= element_text(size = 13), plot.title=element_text(size=18)), 
          f63 + theme(axis.text= element_text(size = 13), plot.title=element_text(size=18)), 
          f66 + theme(axis.text= element_text(size = 13), plot.title=element_text(size=18)), cols=3)


#################################################


### FIG 8.3 (robustness for one-step VAR, with diff_tfp)


one <- read.csv2('fvar_diff_tfp_row1.csv', header=TRUE, sep=',', dec='.')
two <- read.csv2('fvar_diff_tfp_row2.csv', header=TRUE, sep=',', dec='.')
three <- read.csv2('fvar_diff_tfp_row3.csv', header=TRUE, sep=',', dec='.')
four <- read.csv2('fvar_diff_tfp_row4.csv', header=TRUE, sep=',', dec='.')
five <- read.csv2('fvar_diff_tfp_row5.csv', header=TRUE, sep=',', dec='.')
six <- read.csv2('fvar_diff_tfp_row6.csv', header=TRUE, sep=',', dec='.')


## First column:

f11 <- ggplot(one, aes(x=period, y=dln_tfp)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se1), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se2), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(TFP[t] %->% TFP[t])) + theme(plot.title = element_text(size=8))

f21 <- ggplot(two, aes(x=period, y=dln_tfp)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se1), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se2), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(TFP[t] %->% E[t])) + theme(plot.title = element_text(size=10))

f31 <- ggplot(three, aes(x=period, y=dln_tfp)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se1), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se2), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(TFP[t] %->% R[t])) + theme(plot.title = element_text(size=10))

f41 <- ggplot(four, aes(x=period, y=dln_tfp)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se1), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se2), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(TFP[t] %->% P[t])) + theme(plot.title = element_text(size=10))

f51 <- ggplot(five, aes(x=period, y=dln_tfp)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se1), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se2), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(TFP[t] %->% Y[t])) + theme(plot.title = element_text(size=10))

f61 <- ggplot(six, aes(x=period, y=dln_tfp)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se1), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se2), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(TFP[t] %->% Phi[t])) + theme(plot.title = element_text(size=10))


## Second column:

f12 <- ggplot(one, aes(x=period, y=dln_e)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se3), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se4), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(E[t] %->% TFP[t])) + theme(plot.title = element_text(size=10))

f22 <- ggplot(two, aes(x=period, y=dln_e)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se3), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se4), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(E[t] %->% E[t]))

f32 <- ggplot(three, aes(x=period, y=dln_e)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se3), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se4), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(E[t] %->% R[t]))

f42 <- ggplot(four, aes(x=period, y=dln_e)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se3), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se4), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(E[t] %->% P[t]))

f52 <- ggplot(five, aes(x=period, y=dln_e)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se3), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se4), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(E[t] %->% Y[t]))

f62 <- ggplot(six, aes(x=period, y=dln_e)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se3), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se4), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(E[t] %->% Phi[t]))

## Third column:


f13 <- ggplot(one, aes(x=period, y=dln_r)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se5), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se6), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(R[t] %->% TFP[t])) + theme(plot.title = element_text(size=10))

f23 <- ggplot(two, aes(x=period, y=dln_r)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se5), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se6), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(R[t] %->% E[t]))

f33 <- ggplot(three, aes(x=period, y=dln_r)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se5), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se6), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(R[t] %->% R[t]))

f43 <- ggplot(four, aes(x=period, y=dln_r)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se5), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se6), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(R[t] %->% P[t]))

f53 <- ggplot(five, aes(x=period, y=dln_r)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se5), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se6), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(R[t] %->% Y[t]))

f63 <- ggplot(six, aes(x=period, y=dln_r)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se5), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se6), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(R[t] %->% Phi[t]))


# Fourth column:

f14 <- ggplot(one, aes(x=period, y=dln_n)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se7), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se8), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(P[t] %->% TFP[t])) + theme(plot.title = element_text(size=10))

f24 <- ggplot(two, aes(x=period, y=dln_n)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se7), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se8), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(P[t] %->% E[t]))

f34 <- ggplot(three, aes(x=period, y=dln_n)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se7), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se8), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(P[t] %->% R[t]))

f44 <- ggplot(four, aes(x=period, y=dln_n)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se7), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se8), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(P[t] %->% P[t]))

f54 <- ggplot(five, aes(x=period, y=dln_n)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se7), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se8), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(P[t] %->% Y[t]))

f64 <- ggplot(six, aes(x=period, y=dln_n)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se7), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se8), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(P[t] %->% Phi[t]))



# Fifth column:

f15 <- ggplot(one, aes(x=period, y=dln_y)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se9), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se10), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(Y[t] %->% TFP[t])) + theme(plot.title = element_text(size=10))

f25 <- ggplot(two, aes(x=period, y=dln_y)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se9), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se10), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(Y[t] %->% E[t]))

f35 <- ggplot(three, aes(x=period, y=dln_y)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se9), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se10), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(Y[t] %->% R[t]))

f45 <- ggplot(four, aes(x=period, y=dln_y)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se9), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se10), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(Y[t] %->% P[t]))

f55 <- ggplot(five, aes(x=period, y=dln_y)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se9), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se10), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(Y[t] %->% Y[t]))

f65 <- ggplot(six, aes(x=period, y=dln_y)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se9), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se10), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(Y[t] %->% Phi[t]))


## Sixth Column:

f16 <- ggplot(one, aes(x=period, y=dln_b)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se11), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se12), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(Phi[t] %->% TFP[t])) + theme(plot.title = element_text(size=10))

f26 <- ggplot(two, aes(x=period, y=dln_b)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se11), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se12), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(Phi[t] %->% E[t]))

f36 <- ggplot(three, aes(x=period, y=dln_b)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se11), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se12), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(Phi[t] %->% R[t]))

f46 <- ggplot(four, aes(x=period, y=dln_b)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se11), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se12), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(Phi[t] %->% P[t]))

f56 <- ggplot(five, aes(x=period, y=dln_b)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se11), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se12), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(Phi[t] %->% Y[t]))

f66 <- ggplot(six, aes(x=period, y=dln_b)) + 
  geom_line(lwd=1, col='grey45') + 
  geom_line(aes(y=se11), linetype='dashed', col='grey45')  + 
  geom_line(aes(y=se12), linetype='dashed', col='grey45') +
  geom_hline(yintercept = 0) + scale_x_continuous(breaks=pretty_breaks()) +
  labs(x="", y="") + ggtitle(expression(Phi[t] %->% Phi[t]))


multiplot(f11, f21, f31, f41, f51, f61, 
          f12, f22, f32, f42, f52, f62,
          f13, f23, f33, f43, f53, f63,
          f14, f24, f34, f44, f54, f64,
          f15, f25, f35, f45, f55, f65,
          f16, f26, f36, f46, f56, f66, cols=6)


multiplot(f61 + theme(axis.text= element_text(size = 13), plot.title=element_text(size=18)), 
          f64 + theme(axis.text= element_text(size = 13), plot.title=element_text(size=18)),
          f62+ theme(axis.text= element_text(size = 13), plot.title=element_text(size=18)), 
          f65 + theme(axis.text= element_text(size = 13), plot.title=element_text(size=18)), 
          f63 + theme(axis.text= element_text(size = 13), plot.title=element_text(size=18)), 
          f66 + theme(axis.text= element_text(size = 13), plot.title=element_text(size=18)), cols=3)
