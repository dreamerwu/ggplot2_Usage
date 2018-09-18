input=read.delim("~/Desktop/demo.txt",head=T,sep="\t")
ls(input)

#+++++++++++++++++++++++++
# Function to calculate the mean and the standard deviation
# for each group
#+++++++++++++++++++++++++
# data : a data frame
# varname : the name of a column containing the variable
#to be summariezed
# groupnames : vector of column names to be used as
# grouping variables
data_summary <- function(data, varname, groupnames){
    require(plyr)
    summary_func <- function(x, col){
        c(mean = mean(x[[col]], na.rm=TRUE),
          sd = sd(x[[col]], na.rm=TRUE))
    }
    data_sum<-ddply(data, groupnames, .fun=summary_func,
                    varname)
    data_sum <- rename(data_sum, c("mean" = varname))
    return(data_sum)
}
input2 <- data_summary(input, varname="expression", 
                    groupnames=c("group", "gene"))

##########################################################
library(ggplot2)
# Default bar plot
input2$group=factor(input2$group,levels=c("DMSO","Brequinar"))
p<- ggplot(input2, aes(x=gene, y=expression,fill=group)) + 
    scale_fill_manual(values=c("gray","red"))+
    geom_bar(stat="identity", color="black", 
             position=position_dodge()) +
    geom_errorbar(aes(ymin=expression-sd, ymax=expression+sd), width=.2,
                  position=position_dodge(.9)) 
# Finished bar plot
result=p+labs( x="gene name", y = "relative expression")+
    theme(axis.text=element_text(size=10),axis.title=element_text(size=10))+
    theme_classic() 
ggsave("input2.tiff",dpi=600,units=c("cm"),height=3.5,width=5.5)




