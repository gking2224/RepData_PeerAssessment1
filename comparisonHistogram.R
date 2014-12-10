#make histogram of original vs imputed data
#this doesn't work for some reason :- (
``` {r histogram.comparison}
ds.mixed <- data.frame(
    steps=c(ds.na.rm$steps, ds2$steps),
    dataset=as.factor(
        c(rep("original", nrow(ds.na.rm)), rep("imputed", nrow(ds2)))))
gh3 <- ggplot(data=ds.mixed, aes(x=steps,fill=dataset))
gh3 <- gh3 + geom_histogram(binwidth=max(ds.mixed$steps/50),alpha=.5)
print(gh3)
```
