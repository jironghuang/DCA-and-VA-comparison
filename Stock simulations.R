#Spell out motivation of this stduy. Singapore has no existing study on VA and DCA
#Consider US and SG market. US market is to replicate some of Michael Eldeson results

library("FinCal")
library("ggplot2")
#Coding a random walk with drift path

#We have to think the worst case scenario, with driaftand without drift

# set.seed(123456)
# e = rnorm(500); hist(e)  #basically is to simulate a distribution of mean 0 and std dev = 1
# 
# #pure random walk
# rw.nd = cumsum(e); plot(rw.nd)
# trd = 1:500
# 
# #random walk with drift
# rw.wd = 0.5*trd +cumsum(e)
# 
# #deterministic trend and noise
# dt = e+ 0.5*trd
# 
# #plotting
# par(mar = rep(5,4))
# plot.ts(dt,lty = 1, ylab = '', xlab = '')
# lines(rw.wd, lty=2)
# par(new=T)
# plot.ts(rw.nd, lty = 3,axes = FALSE)
# axis(4,pretty(range(rw.nd)))
# lines(rw.nd, lty = 3)
# legend(10,18.7,legend = c('det.trend + noise(ls)','rw drift (ls)','rw (rs)'),lty = c(1,2,3))


####Trying out what Michael eldeson suggested in his book###############
mth_stddev = 0.055
mth_expret = 0.0125

sim_stockprices = function(mth_stddev,mth_expret){

set.seed(1)
r = rnorm(60)
stock_ret = as.data.frame(r*mth_stddev+mth_expret);names(stock_ret) = c("monthly_return"); stock_ret$simulation_num = 1
stock_ret$monthly_return_plusone = stock_ret$monthly_return + 1
stock_ret$cum_return = cumprod(stock_ret$monthly_return_plusone)
stock_ret$month = 1:60

for(i in 2:100){

set.seed(i)
r = rnorm(60)

stock_ret_ind = as.data.frame(r*mth_stddev+mth_expret);names(stock_ret_ind) = c("monthly_return"); stock_ret_ind$simulation_num = i
stock_ret_ind$monthly_return_plusone = stock_ret_ind$monthly_return + 1
stock_ret_ind$cum_return = cumprod(stock_ret_ind$monthly_return_plusone)
stock_ret_ind$month = 1:60
stock_ret = rbind(stock_ret,stock_ret_ind)

}

return(as.data.frame(stock_ret))

stock_ret$simulation_num = as.factor(stock_ret$simulation_num)
ggplot(data = stock_ret, aes(x=month, y=cum_return, group=simulation_num)) +
  geom_line(aes(colour = simulation_num))+
  geom_point(aes(colour = simulation_num),size = 0.1)

}

sim_stockprices(0.055,0.0125)

##########################################

#Generating a 1000 scenarios of 60 normal random distributed series
set.seed(1)
mth_stddev = 0.055
mth_expret = 0.0125

rand_num = as.data.frame(sapply(1:100,function(x) rnorm(59)))
monthly_return_multipler = rand_num*mth_stddev + mth_expret + 1
start = as.data.frame(monthly_return_multipler[1,]); start[1,] = 1
monthly_return_multipler = rbind(start,monthly_return_multipler)

cum_return = cumprod(monthly_return_multipler)
# start = as.data.frame(cum_return[1,]) ; start[1,] = 1
# cum_return = rbind(start,cum_return)


cum_return$period = 1:(nrow(cum_return))

#use reshape function
library(reshape2)
cum_return_long = melt(cum_return,id = c("period"))
ggplot(data = cum_return_long, aes(x=period, y=value, group=variable)) +
  geom_line(aes(colour = variable))+
  geom_point(aes(colour = variable),size = 0.1)

# ggsave("C:/Users/Huang Jirong/Desktop/stock_image.jpeg")


#Dollar-cost strategy, using fv calculation. Or use cumulative product scenario deleting whatever is before the period, then do a cumulative product
#Include dividends?
#Then sum up across dataframes. Each dataframe represents 1 period
#Compare nominal cost invested versus returns
#Compute maximum loss

#Compute maximum gains

#Compute IRR for each different simulation, dollar cost avg easy. Like final profits and cost along the way
# irr(cf=c(-5000,-10000,-6000,-4000,-6500,53000))
#repeat this number of rows
final_index = cum_return[nrow(cum_return),]
final_index_denom = final_index[rep(seq_len(nrow(final_index)),each = nrow(cum_return)),]
profit_multiplier = final_index_denom[,-ncol(final_index_denom)]/cum_return[,-ncol(cum_return)]  #for each period cashflow

##################################
#DCA. Invest for 59 periods, and liquidate on the 60th period
##################################
#Assuming investment of 500 per month
cf = 500

dca_cost = as.data.frame(matrix(cf,nrow = nrow(profit_multiplier), ncol = ncol(profit_multiplier)))
dca = dca_cost*profit_multiplier
dca_fv = as.data.frame(t(as.data.frame(apply(dca,2,sum))))   

#Compute IRR for DCA strategy. 
dca_irr_agg = irr(c(-dca_cost[,1],dca_fv[,1]))
for(i in 1:100){
  dca_irr_ind = irr(c(-dca_cost[,i],dca_fv[,i]))
  dca_irr_agg = c(dca_irr_agg,dca_irr_ind)
}

hist(dca_irr_agg)
##################################

##################################
#VA strategy-->Start with 500 in first period and liquidate in the last period
##################################
#Establish value path (Pg 89)
#VA,t =  500 * t * (1.0125)^t

VA = as.data.frame(profit_multiplier[,ncol(profit_multiplier)])
VA[,1] = 1:(nrow(VA)); names(VA)[1] = "period"
VA$value_path = cf*VA$period * (1+mth_expret) ^(VA$period-1)   #modified last part of formula a bit
# VA$value_path[1] = 0; VA = VA[-1,]           

#Cash flows required
#Use VA path in t - lag of VA path * monthly return multiplier in t
#Create a lag variable
VA$lag_value_path = 0
VA$lag_value_path[2:nrow(VA)] = VA$value_path[1:(nrow(VA)-1)]

#repeated value path
a = as.data.frame(VA$value_path)
VA_rep = as.data.frame(VA$value_path)
for(i in 2:ncol(profit_multiplier)){VA_rep = cbind(VA_rep,a)}

#repeated lagged value path
a = as.data.frame(VA$lag_value_path)
VA_lag_rep = as.data.frame(VA$lag_value_path)
for(i in 2:ncol(profit_multiplier)){VA_lag_rep = cbind(VA_lag_rep,a)}

#CF required to maintain value path
VA_cf = VA_rep - VA_lag_rep * monthly_return_multipler

#Derive VA IRR. Liquidate all at end of 60th period
va_irr_agg = irr(c(-VA_cf[,1],VA$value_path[nrow(VA)]))

for(i in 1:ncol(VA_cf)){
  va_irr_ind = irr(c(-VA_cf[,i],VA$value_path[nrow(VA)]))
  va_irr_agg = c(va_irr_agg,va_irr_ind)
}

hist(va_irr_agg)

#Create a no sell strategy. So if negative, push it to next period, and convert current period cash flow to 0


##################################
#Compare IRR differences using density curves

#Compare IRR differences for each scenario
hist(va_irr_agg - dca_irr_agg)

#Calculate cost to profits. With histogram
#cost
VA_ttl_cost = apply(VA_cf,2,sum)    
dca_ttl_cost = apply(dca_cost,2,sum) 

#Ending value
VA_fv = as.data.frame(t(as.data.frame(rep(VA$value_path[nrow(VA)],ncol(VA_cf)))))
VA_valuepercost = as.numeric(VA_fv/VA_ttl_cost)
dca_valuepercost = as.numeric(dca_fv/dca_ttl_cost)

#naive way of calculating cost without taking into account value of the cost in different period
hist(VA_valuepercost)  
hist(dca_valuepercost)
hist(VA_valuepercost - dca_valuepercost)
summary(VA_valuepercost - dca_valuepercost)

#Taking into account of the cost in each period (pg 123)


#Look at SD across the 100 simulations in ending value or annualized returns

#Compute VAR? exclude for now

#Compute maximum cost required


#####Chi square to test normality assumption############


