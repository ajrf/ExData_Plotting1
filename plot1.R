## Fetchs the data stored in path 'datafile' and generates plot1.png. 
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

## Generate plot1.png
png(filename="plot1.png", width=480, height=480, units="px");
par(mfrow=c(1,1));
hist(dtb$Global_active_power, axes=FALSE, col="red", main="Global Active Power", 
     xlab="Global Active Power (kilowatts)", ylab="Frequency");
axis(1, at=seq(0,6,by=2), labels=seq(0,6,by=2), col.axis="black");
axis(2, at=seq(0,1200,by=200), labels=seq(0,1200,by=200), col.axis="black");
dev.off();
