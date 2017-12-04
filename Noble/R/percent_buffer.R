
# Private function for calculating the percent of wind measurements falling due to wind coming
# through the buffers on either side of the distorted flow field

.percent.buffer=function(site, bgn.month, end.month){
    ## Read in the site info and threshold info:
    thresholds=read.csv("/Users/rlee/Dropbox/GitHub/NEON-FIU-document-IPT/data_store/ATBD_specific_thresholds_2Dwind.csv")

    #get our data
    example.data=Noble::data.pull(site=site,
                                  dpID = "DP1.00001.001",
                                  bgn.month = bgn.month,
                                  end.month = end.month,
                                  time.agr = 30,
                                  package = 'expanded',
                                  save.dir = save.dir)

    # ATBD CALCS
    # Section 5.1
    ## Distorted Flow Test

    ###General parameters

    y_c=thresholds$dy_clockwise[thresholds$SITE==site] #
    y_cc=thresholds$dy_counterClockwise[thresholds$SITE==site] #
    L=thresholds$boomLength[thresholds$SITE==site] #
    x_c=thresholds$dx_clockwise[thresholds$SITE==site] #
    x_cc=thresholds$dx_counterClockwise[thresholds$SITE==site] #

    O_2D = thresholds$boomOrientation[thresholds$SITE==site] # Boom orientation
    B_min = thresholds$Distorted.Flow.Min.Threshold[thresholds$SITE==site] # min distorted flow angle
    B_max = thresholds$Distorted.Flow.Max.Threshold[thresholds$SITE==site] # max distorted flow angle


    #### Wind Field Calculations
    c_d=(abs(atan(y_c/(L+abs(x_c))))*180)/pi # Eqn. 22
    cc_d=(abs(atan(y_cc/(L+abs(x_cc))))*180)/pi # Eqn. 23


    #### Direction thresholds
    D_min = (O_2D-(c_d +B_min)+180)%%360 # Eqn 24
    D_max = (O_2D+(cc_d +B_max)+180)%%360 # Eqn 25

    buff_low=c(D_min, (D_min+B_min)%%360) # Bracketing the low end of distorted flow
    buff_hi = c((D_max-B_max)%%360, D_max) # Bracketing the high end of distorted flow

    LB_dir=example.data$windDirMean.000.010[which(data.table::between(example.data$windDirMean.000.010, lower = buff_low[1], upper=buff_low[2]))]
    UB_dir=example.data$windDirMean.000.010[which(data.table::between(example.data$windDirMean.000.010, lower = buff_hi[1], upper=buff_hi[2]))]

    pcnt.in.buffer=(length(UB_dir)+length(LB_dir))/length(example.data$windDirMean.000.010)*100
    return(pcnt.in.buffer)
}
