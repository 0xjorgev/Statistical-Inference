library(ggplot2)

# load data
data(ToothGrowth)

head(ToothGrowth)

dim(ToothGrowth)

unique(ToothGrowth$supp)
unique(ToothGrowth$dose)

summary(ToothGrowth)

plot <- qplot(supp
              ,len
              ,data=ToothGrowth
              ,facets=~dose
              ,main="Guinea pigs Tooth growth by supplement type and dosage (mg)"
              ,xlab="Supplement type"
              ,ylab="Tooth length") 

plot + geom_boxplot(aes(fill = supp))

# split data set
OJ = ToothGrowth$len[ToothGrowth$supp == 'OJ']
VC = ToothGrowth$len[ToothGrowth$supp == 'VC']

t.test(OJ, VC, alternative = "greater", paired = FALSE, var.equal = FALSE, conf.level = 0.95)

dose05mg = ToothGrowth$len[ToothGrowth$dose == 0.5]
dose1mg = ToothGrowth$len[ToothGrowth$dose == 1]
dose2mg = ToothGrowth$len[ToothGrowth$dose == 2]

t.test(dose05mg, dose1mg, alternative = "less", paired = FALSE, var.equal = FALSE, conf.level = 0.95)

t.test(dose1mg, dose2mg, alternative = "less", paired = FALSE, var.equal = FALSE, conf.level = 0.95)

OJ2 = ToothGrowth$len[ToothGrowth$supp == 'OJ' & ToothGrowth$dose == 2]
VC2 = ToothGrowth$len[ToothGrowth$supp == 'VC' & ToothGrowth$dose == 2]

t.test(OJ2, VC2, alternative = "two.sided", paired = FALSE, var.equal = FALSE, conf.level = 0.95)