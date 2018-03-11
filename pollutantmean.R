pollutantmean<-function(directory,pollutant,id = 1:332){
        
        meaneach<-numeric() 
        n<-numeric()
        
        for(i in seq_along(id)){
                if(id[i]<10){idstring<-paste("00",id[i],sep = "")}
                else if(id[i]>=10 & id[i]<100){idstring<-paste("0",id[i],sep = "")}
                else{idstring<-id[i]}
                
                #"D:/Users/cyingsom/My Documents/R saves/specdata"
                data<-read.csv(paste(directory,"/",idstring,".csv",sep="")) 
                
                info<-data[ !is.na(data[ ,pollutant]), pollutant]
                
                #all pollutant data can be NAs,if that so,
                #length(info) = 0,mean(info)  = NaN
                #mean(info)=NaN will cause problem to find result
                #so we have to write if-else condition
                
                if(length(info)!=0){ 
                meaneach[i]<-mean(info)
                n[i]<-length(info)}
                
                else{meaneach[i]<-0
                     n[i]<-0  } 
                        
        }
        
      sum(meaneach*n) / sum(n)  #weighted mean

        
}