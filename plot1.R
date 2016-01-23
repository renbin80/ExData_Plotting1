plot1<-function(){
     ## Make sure directory is correct
     setwd("/home/renbin/Desktop/R/Exploratory data analysis course project 1")

     ## File
     file <- "household_power_consumption.txt"
     
     ## Create connection
     con <- file(description=file, open="r")
     
     end<-FALSE

     ## Read first line of file as one character string
     strlist<-scan(file=con, what= character(), nlines=1, quiet=TRUE)
     ## Split line into individual column names
     strlist<-strsplit(strlist, ";")
     
     ## Create a matrix with no rows but with number of columns (9)
     m<-matrix(, ,nrow=0,ncol=length(unlist(strlist)))
     
     ## Assign column names to matrix
     colnames(m)<-unlist(strlist)

     ## Starts while loop to extract subsequent observations
     while(end!=TRUE){

          ## Scan a line from the file and split it into a list
          tmp <- strsplit(scan(file=con, what= character(), nlines=1, quiet=TRUE),";")

          ## Put list into a character vector
          tmp<-unlist(tmp)

          ## Get first column of vector and check if it is later than 3rd Feb 2007. If TRUE, exit while loop
          if(as.Date(tmp[1], format="%d/%m/%Y")>=as.Date("2007-02-03")) {
               end<-TRUE
               break
          }

          ## Get first column of vector and check if it is later than 1st Feb 2007. If TRUE, add to row in matrix
          if(as.Date(tmp[1], format="%d/%m/%Y")>=as.Date("2007-02-01")){
               m<-rbind(m,tmp)         
          }           
     }     
     
     ## Close file connection
     close(con) 
     
     ## Get rid of unwanted row names
     rownames(m)<-NULL
     
     ## Convert matrix into data frame
     m<-as.data.frame(m)
     
     ## Open graphics device for PNG
     png("plot1.png", units = "px", width=480, height=480)
     
     ## Plot graph
     hist(as.numeric(as.character(m$Global_active_power)), col="red", xlab="Global Active Power (kilowatts)", main="Global Active Power", breaks=12)

     ## close graphic device to save PNG
     dev.off()

     ## Return the data frame for further processing      
     m     
     
}