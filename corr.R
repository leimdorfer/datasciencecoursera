build_ID <- function(id){
      
      if (id<10){            
            zeros<-"00"            
            
      } else if (id<100){          
            zeros<-"0"
            
      } else {         
            zeros<-""
      }
      toString(id)
      
      this.id <- paste(c(zeros,id,".csv"), collapse='')
      
      this.id
}

return_data <- function(directory,id){
      
      file_as_string <- build_ID(id)
      
      file_to_read <- paste(c(directory,"/",file_as_string),collapse='')
      
      data <- read.csv(file_to_read)
      
      data
      
}

return_correlations <- function(directory, threshold, id = 1:332) {
      
      id_from_arg = id
            
      count<-1
      
      for (i in id_from_arg){     
            
            data <- return_data(directory, i) # returns df from directory/id
            
            good <- complete.cases(data)
            
            data.valid <- data[good,]
            
            valid.rows.count <- nrow(data.valid) 
            
            if(valid.rows.count>threshold){ ## only data that meets threshold
                  
                  sulfate <- data.valid[, c(2)]
                  nitrate <- data.valid[, c(3)]
                  
                  this.cor <- cor(sulfate,nitrate)
                  
                  if(count==1){
                        
                        output.corrs<-c(this.cor)
                        
                  }else{
                        
                        output.corrs<-c(output.corrs,this.cor)                        
                  }
                  
                  count <- count+1 
                  
            } else {
                  
                  output.corrs<-numeric()
                  
            }  
      }
      
      output.corrs
}

corr <- function(directory, threshold = 0) {
	      
      output <- return_correlations(directory, threshold)
      
      output

}