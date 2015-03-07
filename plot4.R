## Fetchs the data stored in path 'datafile' and generates plot4.png. 
datafile="household_power_consumption.txt";

## FLAGS: 
#   - fullData: 
#       * if TRUE, loads the complete dataset and subsets 
#       the 2-day period of interest [slow];
#       * if FALSE, loads the data correspoding to 
#       the 2-day period of interest [fast].        
#       (The 2-day period of interest is 2007-02-01 and 2007-02-02, 
#       which correspond to lines 66638 to 69517; 2880 lines = 48*60 minutes.) 
#   - hasDataTablePckg:
#       * if TRUE, the 'data.table' package has been loaded 
#       and the fast 'fread' method for loading the data is used; 
#       * if FALSE, the 'data.table' package is not present 
#       and the slow 'read.csv' method for loading the data is used. 
fullData<-FALSE;
hasDataTablePckg<-require(data.table);

## Loads data
# In the data there are rows with ? (= NA) values in would-have-been 'numeric' columns; 
# In the period of interest all non-date-time columns have 'numeric' values. 
if (fullData){ # slow
    if (hasDataTablePckg){ # fast
        dtb<-fread(datafile, sep=";", na.strings="?", 
                   colClasses=c(rep("character",2),rep(NA,7))); 
        # non-date-time columns cannot be numeric due to ? (= NA) values, 
        # and coercion does not work in fread; do now the coercion:  
        for (i in 3:9){ class(dtb[[i]])<-"numeric"; }
    } else { # slow
        dtb<-read.csv(datafile, sep=";", na.strings="?", 
                      colClasses=c(rep("character",2),rep("numeric",7)));
    }
    # subsetting the rows of interest
    dtb<-subset(dtb, strptime(Date,"%d/%m/%Y")>=as.POSIXlt("2007-02-01") & 
                     strptime(Date,"%d/%m/%Y")<=as.POSIXlt("2007-02-02"));
} else { # fast
    # Read lines 66638 to 69517 (2880 rows = 48*60 minutes)
    if (hasDataTablePckg){ # fast
        dtb<-fread(datafile, sep=";", na.strings="?", header=FALSE, skip=66637, 
                   nrows=2880, colClasses=c(rep("character",2),rep("numeric",7)));
        # load the column headers
        setattr(dtb, "names", as.character(
                fread(datafile,sep=";",header=FALSE,nrows=1)
            ));
    } else { # slow
        dtb<-read.csv(datafile, sep=";", na.strings="?", header=FALSE, skip=66637, 
                      nrows=2880, colClasses=c(rep("character",2),rep("numeric",7)));
        # load the column headers
        names(dtb)<-as.character(
                read.csv(datafile,sep=";",header=FALSE,nrows=1, stringsAsFactors=FALSE)
            );
    }
}

# A date-time vector from merging the data's 'Date' and 'Time' columns, 
# to be used as x-values in the plot. 
dateTimeSeries<-strptime(paste(dtb$Date, dtb$Time, sep=" "), "%d/%m/%Y %H:%M:%S");

## Generate plot4.png
png(filename="plot4.png", width=480, height=480, units="px");
par(mfrow=c(2,2));
cols=c("black", "red", "blue");
with(dtb,{
    plot(dateTimeSeries, Global_active_power, 
         xlab="", ylab="Global Active Power", type="l");
    plot(dateTimeSeries, Voltage, xlab="datetime", ylab="Voltage", type="l");
    plot(dateTimeSeries, Sub_metering_1, 
         xlab="", ylab="Energy sub metering", type="l", col=cols[1]);
    lines(dateTimeSeries, Sub_metering_2, col=cols[2]);
    lines(dateTimeSeries, Sub_metering_3, col=cols[3]);
    legend(x="topright", box.col=NA, bg=NA, legend=names(dtb)[7:9], col=cols, lwd=1);
    plot(dateTimeSeries, Global_reactive_power, 
         xlab="datetime", ylab="Global_reactive_power", type="l", yaxt="n");
    axis(2, at=seq(0.0,0.5,by=0.1), labels=c("0.0",seq(0.1,0.5,by=0.1)), 
         col.axis="black");    
})
dev.off();
