# April 8 Notes
library(readstata13)
library(zoo)
library(ggplot2)

DHS = read.dta13("DavisHomeSale.dta")
year_monstring = paste0(DHS$year,"-",DHS$month)

DHS$yearmonth = as.yearmon(year_monstring)
DHS$t = 1:nrow(DHS)
results = lm(salecount~t, data = DHS)
summary(results)
DHS$salecounthat = predict(results)


pl1 = ggplot(data = DHS, aes(x = yearmonth, y = salecount)) + geom_line() + theme_bw() +
  scale_y_continuous("Number of Houses Sold") + scale_x_yearmon("Time") + ggtitle("Number of Houses Sold over Time") +
  geom_smooth(method = "lm", formula = "y~x", se = F, color = "red", linetype = "dashed")
print(pl1)

# TREND
results2 = lm(salecount ~ t + factor(month), data = DHS)
DHS$salecounthat2 = predict(results2)

df1 = cbind.data.frame(X = DHS$t, Y = DHS$salecount, Type = "Real")
tobind = cbind.data.frame(X = DHS$t, Y = DHS$salecounthat2, Type = "Predicted")
df2 = rbind.data.frame(df1, tobind)
pl2 = ggplot(data = subset(df2, Type == "Real"), aes(x = X, y = Y)) + geom_line() + 
  theme_bw() + geom_line(data = subset(df2, Type = "Predicted"), linetype = "dashed", color = "red") + 
  scale_y_continuous("Number of Houses Sold") + scale_x_continuous("Time")
print(pl2)
  



