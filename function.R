require(data.table)
require(dygraphs)

scon <- fread("D:/New folder (7)/matr/SCONES_test.tsv")  ##reading data ser in tabular foramt

visualize <- function(x) {

      x<-data.frame(x)    #converting into dataframe
      
      cat("(choose two for beeter visualization in parts)")
      
      n <- readline(prompt="Enter number of plots you need for each sample 1 or 2: ")
      n <- as.integer(n)
      
      # separating two data samples
        x1 <- x[x$chr == 1, ]
        x2 <- x[x$chr != 1, ]

      
      #converting data frame
    t1<-data.frame("range"=1:nrow(x1))
    t2<-data.frame("range"=1:nrow(x2))
  
    t1$testsample_first<-exp(x1$testSample1)
    t1$testsample_second<-exp(x1$testSample2)
    
  
    t2$testsample_first<-exp(x2$testSample1)
    t2$testsample_second<-exp(x2$testSample2)
   
    
    
      if(n==1){
        list(dygraph(t1%>%  dyRangeSelector(),main = "sample_one"),
             dygraph(t2%>%  dyRangeSelector(),main = "sample_two"))
        
        
      }
    
    else if(n==2){
      
      pt1<-t1[1:4000,]
      pt2<-t1[4001:nrow(t1),]
      
      ptt1<-t2[1:4000,]
      ptt2<-t2[4001:nrow(t2),]
      
      
      list(dygraph(pt1 %>%  dyRangeSelector(),main = "sample_one upto 4000"),
           dygraph(pt2 %>%  dyRangeSelector(),main = "sample_one 4001 onwards"),
           dygraph(ptt1 %>%  dyRangeSelector(),main = "sample_two upto 4000"),
           dygraph(ptt2 %>%  dyRangeSelector(),main = "Sample_two 4001 onwards"))
    }
    else
      {(cat("your entry is wrong ."))}
    
}


(visualize(scon))

