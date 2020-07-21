####### OUTSOURCED GRAPH FUNCTIONS FROM RMARKDOWN

# Written by A.E. Hausmann (last update April 29 2020)

#Open range plot for plotting of the interaction terms.
#This simply creates an empty plot and adds axes to it.
open_range_plot<-function(xlim_range,par_mar=c(0.2,1,0.2,2),tck0.5=0.02,ylim_range=c(0,1)){
  par(mar=par_mar)
  plot(1,1,xlim=xlim_range,
       ylim=ylim_range,type="n",pch=21,xlab="",ylab="",xaxt="n",yaxt="n")
  # axis(2,labels=F,lwd=0,lwd.ticks=2,at=0.5,tck=tck0.5)
  # axis(4,labels=F,lwd=0,lwd.ticks=2,at=0.5,tck=tck0.5)
}

#Add CI or CrI.
#This plots the credibility interval for each interaction term.
add_polygon<-function(x_col,low_y,up_y,data,colname_subset,sub_name,colo){
  polygon(c(data[,x_col][data[,colname_subset]==sub_name],
            rev(data[,x_col][data[,colname_subset]==sub_name])),
          c(data[,low_y][data[,colname_subset]==sub_name],
            rev(data[,up_y][data[,colname_subset]==sub_name])),
          border=NA,col=adjustcolor(colo,1/3))
}

#This adds the model estimators for the interaction terms.
add_lines<-function(x_col,y_col,colo,lwdd=2,ltyy="solid",data,colname_subset=NA,sub_name=NA){
  if(is.na(colname_subset)){
    lines(data[,x_col],data[,y_col],
          col=colo,lwd=lwdd,lty=ltyy)
  } else{
    lines(data[,x_col][data[,colname_subset]==sub_name],data[,y_col][data[,colname_subset]==sub_name],
          col=colo,lwd=lwdd,lty=ltyy)
  }
}

#This adds a preference score at the y-axis
add_yaxs<-function(sideaxs=4,numbers=T){
  if(sideaxs%in%1:4){
    axis(sideaxs,at=c(0,0.25,0.5,0.75,1),labels=F,tck=-0.015,lwd=1)
    if(numbers){
      axis(sideaxs,at=c(0,0.25,0.5,0.75,1),c("0","0.25","0.5","0.75","1"),las=1,cex.axis=0.85,line=-0.45,lwd=0)
    }
  }
}

#This is to add a x axis to a interaction term plot
xaxs_interact<-function(add_title=T,added_title,added_title_cex=0.8,
                        xvalues=c(1.5,5,15,50,150)){
  
  #Define values
  xvalues=xvalues*1000
  
  #extend this by the opposite ratios and take logarithm
  xvalues_range<-log10(xvalues)
  #Plot these at the position where they are supposed to go on the new axis.
  axis(1,at=xvalues_range,
       c(paste0(xvalues/1000,"k")),
       line=-0.7,lwd=0,cex.axis=0.85)
  #Add ticks
  axis(1,at=c(xvalues_range),labels=F,lwd=0,lwd.ticks=1,tck=-0.02)
  
  if(add_title){
    mtext(added_title,
          1,line=1.8,cex=added_title_cex)
  }
}

#This is the main function to plot an interaction term
plot_interaction<-function(covariate,
                           bay_cov1,
                           added_title,
                           yaxs_side=2,
                           par_mar1=c(3,1,0.5,0.05),
                           ylabel="",
                           added_title_cex=0.8,
                           add0.5=F,
                           add_empty=F,
                           add_letter="A",
                           letter_central=F,
                           add_xaxs=T,
                           text_inner=F,
                           text_inner_fill=substitute(paste(italic("H. heurippa")," vs ",italic("H. t. linaresi"))),
                           ylabel_outer=T,
                           ylabel_line=1.5,
                           numbers=T,
                           xvalues=c(1.5,5,15,50,150),
                           colname_subset="type",
                           sub_names=c("TLC","HC"),
                           custom_xlim=c()){
  
  #Get the extent for x values of the combined data set
  if(length(custom_xlim)>0){
    all_values_x<-custom_xlim
  } else{
    all_values_x<-range(c(bay_cov1[,covariate]))
  }
  
  #Open empty plot
  open_range_plot(xlim_range=all_values_x,
                  par_mar = par_mar1,
                  ylim_range=c(0.24,0.76),
                  tck0.5=0)
  
  #This adds the comparison of interaction terms either as text within plot or above plot
  if(text_inner){
    text(par("usr")[2]-0.07*diff(par("usr")[1:2]),
         par("usr")[4]-0.1*diff(par("usr")[3:4]),
         text_inner_fill,
         pos=2,offset=0)
  } else{
    mtext(text_inner_fill,
          3,line=0.1,cex=0.65)
  }
  
  #This adds a line at 0.5
  if(add0.5){
    abline(h=0.5,lty="dashed",lwd=0.5)
  }
  
  #Add credibility interval of interaction
  add_polygon(x_col=covariate,low_y="lower",up_y="upper",data=bay_cov1,colname_subset=colname_subset,sub_name=sub_names[1],colo="blue")
  add_polygon(x_col=covariate,low_y="lower",up_y="upper",data=bay_cov1,colname_subset=colname_subset,sub_name=sub_names[2],colo="red")
  
  #Add estimate of interaction
  add_lines(x_col=covariate,y_col="estimate",colo="blue",data=bay_cov1,colname_subset=colname_subset,sub_name=sub_names[1])
  add_lines(x_col=covariate,y_col="estimate",colo="red",data=bay_cov1,colname_subset=colname_subset,sub_name=sub_names[2])
  
  #Draw thicker box around plot
  box(,lwd=1.4)
  
  #Add y axis
  add_yaxs(sideaxs=yaxs_side[1],numbers=numbers)
  
  #Add panel letter
  if(add_letter!=""){
    if(letter_central){
      text(par("usr")[1]+0.5*diff(par("usr")[1:2]),
           par("usr")[4]-(0.15/par("pin")[2])*diff(par("usr")[3:4]),
           add_letter,font=2,cex=1.5)
    } else{
      text(par("usr")[1]+(0.2/par("pin")[1])*diff(par("usr")[1:2]),
           par("usr")[4]-(0.15/par("pin")[2])*diff(par("usr")[3:4]),
           add_letter,font=2,cex=1.5,
           pos=2,offset=0)
    }
  }
  
  #Add x axis
  if(add_xaxs){
    xaxs_interact(add_title=T,added_title=added_title,added_title_cex=added_title_cex,
                  xvalues=xvalues)
  }
  
  #Add axis title
  mtext(ylabel,2,outer=ylabel_outer,cex=added_title_cex,line=ylabel_line)
  
  #If requested, add meaningless empty plot at end
  if(add_empty){
    old_mar<-par("mar")
    par(mar=c(0,0.4,1.6,0.4))
    plot(1,1,bty="n",type="n",xaxt="n",yaxt="n",xlab="",ylab="")
    par(mar=old_mar)
  }
  
}

#This function adds the right panel of a Gardner-Altman plot. We have to provide
#the posterior for lin, for heu, for the difference, as well as the histogram for the difference.
#Additionally, we can decide whether we want to add an axis title or not
add_gardner_altman<-function(TLC_posterior,HC_posterior,difference_posterior,
                             post_hist,add_title=T,at_pos=c(0.5,1.5)){
  
  #Calculate the mean for each type. We calculate the mean and not the median
  #because only the mean allows that the Gardner-Altman plot works out precisely
  TLC_pred<-mean(TLC_posterior)
  HC_pred<-mean(HC_posterior)
  
  #Get the 95% equal-tailed credible interval and the median of the difference posterior
  diff_cri<-c(quantile(difference_posterior,probs=c(0.025,0.975)),mean(difference_posterior))[c(1,3,2)]
  
  #Add a second y-axis on the right side of the plot. We determine first which values
  #Between -1 and 1 (and a step sequence of 0.1) lie within the plotting range
  exist_0.1<-seq(-1,1,0.1)[TLC_pred+seq(-1,1,0.1)>=0&TLC_pred+seq(-1,1,0.1)<=1]
  #Add axis
  axis(4,at=TLC_pred+exist_0.1,exist_0.1,labels=F,tck=-0.015,lwd=1,line=1.8)
  axis(4,at=TLC_pred+exist_0.1,exist_0.1,las=1,cex.axis=0.85,line=1.35,lwd=0)
  
  par(xpd=T)
  #Remove right side of plot box
  abline(v=par("usr")[2],col="white",lwd=1.4)
  
  #Add means of posteriors for the two types. First just add a white background (black line later)
  segments(at_pos,
           c(TLC_pred,HC_pred),
           rep(par("usr")[2]+(par("cex")*1.7*par("cin")[2]/par("pin")[1])*diff(par("usr")[1:2]),2),
           c(TLC_pred,HC_pred),
           lwd=2.5,col="white",lend=3)
  
  #Add posteriors of differences
  invisible(sapply(1:length(post_hist$counts),function(x) if(post_hist$counts[x]>0){polygon(c(par("usr")[2],par("usr")[2]+post_hist$counts[x],
                                                                                              par("usr")[2]+post_hist$counts[x],par("usr")[2]),
                                                                                            TLC_pred+c(rep(post_hist$breaks[x],2),
                                                                                                       rep(post_hist$breaks[x+1],2)),
                                                                                            border=NA,col="gray")}))
  #Add Cri for difference
  segments(par("usr")[2],TLC_pred+diff_cri[1],
           par("usr")[2],TLC_pred+diff_cri[3],lwd=3,lend=3)
  #Add mean difference
  points(par("usr")[2],TLC_pred+diff_cri[2],pch=20,cex=2)
  
  #Add means of posteriors for the two types.
  segments(at_pos,
           c(TLC_pred,HC_pred),
           rep(par("usr")[2]+(par("cex")*1.8*par("cin")[2]/par("pin")[1])*diff(par("usr")[1:2]),2),
           c(TLC_pred,HC_pred),
           lwd=1.3,lend=3)
  
  #If wished, we add an axis title
  if(add_title){
    text(par("usr")[2]+(par("cex")*4.4*par("cin")[2]/par("pin")[1])*diff(par("usr")[1:2]),
         0.5,expression(Delta ~ "Proportion"),srt = 270,
         font=2,cex=1)
  }
  
  par(xpd=F)
  
}


#This function draws the colour scale for preferences
add_scale<-function(xprop_start=0.10, #Start point (x-direction) as units of x-axis away from plot
                    xprop_end=0.22, #End point (x-direction) as units of x-axis away from plot
                    col_range=c(0.2,0.8) #Range of preference values
){
  
  #Now we draw the legend
  #Some measures to understand the plot dimensions
  xextent<-par("usr")[1:2]
  yextent<-par("usr")[3:4]
  prop_bottom<-sum(par("mai")[c(1,3)])/par("pin")[2]
  
  #Define colour ramp
  paletto<-colorRampPalette(c("blue", "red"))
  #Sample 1000 colours
  bluered<-paletto(1000)
  
  par(xpd=NA)
  
  #Extents and step size of scale
  ybottom<-mean(yextent[1:2])+0.5*prop_bottom*abs(diff(yextent[1:2]))
  ytop<-mean(yextent[1:2])+(1+0.5*prop_bottom)*abs(diff(yextent[1:2]))
  stepsize<-(abs(diff(yextent[1:2])))/999
  
  #Paste scale
  invisible(sapply(1:length(bluered),function(x) polygon(c(xextent[2]+xprop_start*abs(diff(xextent)),
                                                           xextent[2]+xprop_end*abs(diff(xextent)),
                                                           xextent[2]+xprop_start*abs(diff(xextent)),
                                                           xextent[2]+xprop_end*abs(diff(xextent))),
                                                         c(seq(ybottom,ytop,stepsize)[x]-0.05*stepsize,
                                                           seq(ybottom,ytop,stepsize)[x]-0.05*stepsize,
                                                           seq(ybottom,ytop,stepsize)[x+1]-0.05*stepsize,
                                                           seq(ybottom,ytop,stepsize)[x+1]-0.05*stepsize),
                                                         border=NA,col=bluered[x])))
  #Draw outline
  polygon(c(xextent[2]+xprop_start*abs(diff(xextent)),
            xextent[2]+xprop_end*abs(diff(xextent)),
            xextent[2]+xprop_end*abs(diff(xextent)),
            xextent[2]+xprop_start*abs(diff(xextent))),
          c(ybottom-0.05*stepsize,
            ybottom-0.05*stepsize,
            ytop-0.05*stepsize,
            ytop-0.05*stepsize))
  
  #Add "axis"
  segments(c(xextent[2]+xprop_end*abs(diff(xextent)),
             xextent[2]+xprop_end*abs(diff(xextent)),
             xextent[2]+xprop_end*abs(diff(xextent))),
           c(ybottom-0.05*stepsize,
             mean(yextent[1:2])+(0.5+0.5*prop_bottom)*abs(diff(yextent[1:2]))-0.05*stepsize,
             ytop-0.05*stepsize),
           c(xextent[2]+(xprop_end+0.02)*abs(diff(xextent)),
             xextent[2]+(xprop_end+0.02)*abs(diff(xextent)),
             xextent[2]+(xprop_end+0.02)*abs(diff(xextent))),
           c(ybottom-0.05*stepsize,
             mean(yextent[1:2])+(0.5+0.5*prop_bottom)*abs(diff(yextent[1:2]))-0.05*stepsize,
             ytop-0.05*stepsize))
  
  #Write 0, 0.5 and 1 next to the scale
  text(c(xextent[2]+(xprop_end+0.07)*abs(diff(xextent)),
         xextent[2]+(xprop_end+0.07)*abs(diff(xextent)),
         xextent[2]+(xprop_end+0.07)*abs(diff(xextent))),
       c(ybottom-0.05*stepsize,
         mean(yextent[1:2])+(0.5+0.5*prop_bottom)*abs(diff(yextent[1:2]))-0.05*stepsize,
         ytop-0.05*stepsize),
       c(as.character(col_range[1]),"0.5",as.character(col_range[2])),cex=0.8)
  
  par(xpd=F)
}


#This is the main function to plot the three-way interaction
interact3<-function(col_range=c(0.2,0.8)){
  
  #Define axis labels
  xaxis_val<-c(1.5,5,15,50,150)
  
  #Minimum and maximum lux measure at the heu female for heu and lin males
  min1<-min(c(pref_stat_TLC_HC$log_light_HC,
              pref_stat_BC$log_light_HC),na.rm=T)
  max1<-max(c(pref_stat_TLC_HC$log_light_HC,
              pref_stat_BC$log_light_HC),na.rm=T)
  #Minimum and maximum lux measure at the lin female for heu and lin males
  min2<-min(c(pref_stat_TLC_HC$log_light_TLC,
              pref_stat_BC$log_light_TLC),na.rm=T)
  max2<-max(c(pref_stat_TLC_HC$log_light_TLC,
              pref_stat_BC$log_light_TLC),na.rm=T)
  
  #Change scale of min and max to centered and scaled variable for heu and lin male dataset
  min1_trans<-as.numeric(scale(min1,attr(pref_stat_TLC_HC$log_light_HC.z,"scaled:center"),attr(pref_stat_TLC_HC$log_light_HC.z,"scaled:scale")))
  max1_trans<-as.numeric(scale(max1,attr(pref_stat_TLC_HC$log_light_HC.z,"scaled:center"),attr(pref_stat_TLC_HC$log_light_HC.z,"scaled:scale")))
  
  min2_trans<-as.numeric(scale(min2,attr(pref_stat_TLC_HC$log_light_TLC.z,"scaled:center"),attr(pref_stat_TLC_HC$log_light_TLC.z,"scaled:scale")))
  max2_trans<-as.numeric(scale(max2,attr(pref_stat_TLC_HC$log_light_TLC.z,"scaled:center"),attr(pref_stat_TLC_HC$log_light_TLC.z,"scaled:scale")))
  
  #Number of sample intervals between the extremes
  n<-50
  
  #Step size for intervals
  by1<-abs(diff(c(min1_trans,max1_trans)))/(n-1)
  by2<-abs(diff(c(min2_trans,max2_trans)))/(n-1)
  
  #Define newdata for later predict function. Sample along the minima and
  #maxima of the respective light measures for lin males.
  grdi1<-expand.grid(log_light_HC.z=seq(min1_trans,max1_trans,by1),
                     log_light_TLC.z=seq(min2_trans,max2_trans,by2),
                     type="TLC")
  #Now predict
  predo1<-predict(mod1.2,grdi1,type="response",re_formula = NULL,
                  allow_new_levels =T)[,1]
  #Same definition of newdata, now for heu males
  grdi2<-expand.grid(log_light_HC.z=seq(min1_trans,max1_trans,by1),
                     log_light_TLC.z=seq(min2_trans,max2_trans,by2),
                     type="HC")
  #Prediction for heu males
  predo2<-predict(mod1.2,grdi2,type="response",re_formula = NULL,
                  allow_new_levels =T)[,1]
  
  #Now repeat the same for the backcross males!
  #Rescale
  min1_trans_n<-as.numeric(scale(min1,attr(pref_stat_BC$log_light_HC.z,"scaled:center"),attr(pref_stat_BC$log_light_HC.z,"scaled:scale")))
  max1_trans_n<-as.numeric(scale(max1,attr(pref_stat_BC$log_light_HC.z,"scaled:center"),attr(pref_stat_BC$log_light_HC.z,"scaled:scale")))
  
  min2_trans_n<-as.numeric(scale(min2,attr(pref_stat_BC$log_light_TLC.z,"scaled:center"),attr(pref_stat_BC$log_light_TLC.z,"scaled:scale")))
  max2_trans_n<-as.numeric(scale(max2,attr(pref_stat_BC$log_light_TLC.z,"scaled:center"),attr(pref_stat_BC$log_light_TLC.z,"scaled:scale")))
  
  #Step size
  by1_n<-abs(diff(c(min1_trans_n,max1_trans_n)))/(n-1)
  by2_n<-abs(diff(c(min2_trans_n,max2_trans_n)))/(n-1)
  
  #Newdata for non-red males
  grdi3<-expand.grid(log_light_HC.z=seq(min1_trans_n,max1_trans_n,by1_n),
                     log_light_TLC.z=seq(min2_trans_n,max2_trans_n,by2_n),
                     redYN="N")
  #Prediction
  predo3<-predict(mod2.2,grdi3,type="response",re_formula = NULL,
                  allow_new_levels =T)[,1]
  
  #Newdata for males with red
  grdi4<-expand.grid(log_light_HC.z=seq(min1_trans_n,max1_trans_n,by1_n),
                     log_light_TLC.z=seq(min2_trans_n,max2_trans_n,by2_n),
                     redYN="Y")
  #Prediction
  predo4<-predict(mod2.2,grdi4,type="response",re_formula = NULL,
                  allow_new_levels =T)[,1]
  
  #Define range for colour palette
  rango<-col_range
  #Define colour ramp
  paletto<-colorRampPalette(c("blue", "red"))
  #Sample 1000 colours
  bluered<-paletto(1000)
  
  #inner margins
  par(mar=c(0,0.4,1.6,0.4))
  
  
  #lin males:
  
  #Open plot fitting the ranges of prediction
  plot(1,1,ylim=c(min2_trans-by2,max2_trans+by2),
       xlim=c(min1_trans-by1,max1_trans+by1),xaxs="i",yaxs="i",xaxt="n",
       yaxt="n",xlab="",ylab="",type="n")
  #Give each prediction a colour (red for high preference for heu, blue
  #for hight preference for lin)
  col_dots<-bluered[sapply(1:length(predo1),function(x) which.min(abs(seq(rango[1],rango[2],(1/999)*diff(rango))-predo1[x])))]
  #Add colour landscape
  invisible(sapply(1:length(predo1),function(x) polygon(c(grdi1[x,1]-by1,grdi1[x,1]+by1,
                                                          grdi1[x,1]+by1,grdi1[x,1]-by1),
                                                        c(grdi1[x,2]-by2,grdi1[x,2]-by2,
                                                          grdi1[x,2]+by2,grdi1[x,2]+by2),
                                                        border=NA,col=col_dots[x])))
  #Number of "height" levels
  ncat=20
  #Calculate weighted Kernel density. Weights are passed as the expected preference
  #for the current phenotype (i.e. the higher the preference value, the higher the density)
  k2d_out<-kde2d.weighted(grdi1[,1],
                          grdi1[,2], 
                          n = ncat, 
                          w=predo1)
  #Get range of Kernel
  range_z<-range(k2d_out$z)
  #Define levels of mountain ridges
  pretty_0.004<-c(seq(0,0.996,0.004)[range_z[1]>seq(0,0.996,0.004)&range_z[1]<seq(0.004,1,0.004)],
                  seq(0.004,1,0.004)[range_z[2]<seq(0.004,1,0.004)&range_z[2]>seq(0,0.996,0.004)])
  #Draw contour
  contour(k2d_out, drawlabels=FALSE, levels=seq(pretty_0.004[1],pretty_0.004[2],0.004),add=TRUE,lwd=0.6,lty="dashed",col="white")
  
  text(mean(par("usr")[1:2]),
       par("usr")[4]-(0.15/par("pin")[2])*diff(par("usr")[3:4]),
       "E",font=2,cex=1.5,
       col="white")
  
  #Add label
  mtext(substitute(paste(italic('H. t. linaresi'))),3,line=0.1,cex=0.65)
  
  #Add yaxis
  yaxis_val.z<-as.numeric(scale(log10(xaxis_val*1000),attr(pref_stat_TLC_HC$log_light_TLC.z,"scaled:center"),attr(pref_stat_TLC_HC$log_light_TLC.z,"scaled:scale")))
  axis(2,at=yaxis_val.z,paste0(xaxis_val,"k"),line=-0.4,lwd=0,cex.axis=0.85,las=2)
  axis(2,at=yaxis_val.z,labels=F,tck=-0.03)
  
  #Add xaxis
  mtext(substitute(paste("Illuminance at mounted  ",italic('H. t. linaresi')," female [",italic('lux'),"]","                                                         ")),2,line=2,cex=0.65)
  
  
  #Repeat for heu!
  
  par(mar=c(0,0.4,1.6,0.4))
  plot(1,1,ylim=c(min2_trans-by2,max2_trans+by2),
       xlim=c(min1_trans-by1,max1_trans+by1),xaxs="i",yaxs="i",xaxt="n",
       yaxt="n",xlab="",ylab="",type="n")
  col_dots<-bluered[sapply(1:length(predo2),function(x) which.min(abs(seq(rango[1],rango[2],(1/999)*diff(rango))-predo2[x])))]
  invisible(sapply(1:length(predo2),function(x) polygon(c(grdi2[x,1]-by1,grdi2[x,1]+by1,
                                                          grdi2[x,1]+by1,grdi2[x,1]-by1),
                                                        c(grdi2[x,2]-by2,grdi2[x,2]-by2,
                                                          grdi2[x,2]+by2,grdi2[x,2]+by2),
                                                        border=NA,col=col_dots[x])))
  ncat=20
  k2d_out<-kde2d.weighted(grdi2[,1],
                          grdi2[,2], 
                          n = ncat, 
                          w=predo2)
  range_z<-range(k2d_out$z)
  pretty_0.004<-c(seq(0,0.996,0.004)[range_z[1]>seq(0,0.996,0.004)&range_z[1]<seq(0.004,1,0.004)],
                  seq(0.004,1,0.004)[range_z[2]<seq(0.004,1,0.004)&range_z[2]>seq(0,0.996,0.004)])
  contour(k2d_out, drawlabels=FALSE, levels=seq(pretty_0.004[1],pretty_0.004[2],0.004),add=TRUE,lwd=0.6,lty="dashed",col="white")
  
  text(mean(par("usr")[1:2]),
       par("usr")[4]-(0.15/par("pin")[2])*diff(par("usr")[3:4]),
       "F",font=2,cex=1.5,
       col="white")
  
  mtext(substitute(paste(italic('H. heurippa'))),3,line=0.1,cex=0.65)
  
  # par(xpd=NA)
  # text(par("usr")[2]+diff(par("usr")[1:2])*((par("mai")[4]/par("pin")[1])+0.15),par("usr")[4]+0.05*diff(par("usr")[3:4]),"C",font=2,cex=2)
  # par(xpd=F)
  
  
  #Backcrosses
  
  par(mar=c(0,0.4,1.6,0.4))
  plot(1,1,ylim=c(min2_trans_n-by2_n,max2_trans_n+by2_n),
       xlim=c(min1_trans_n-by1_n,max1_trans_n+by1_n),xaxs="i",yaxs="i",xaxt="n",
       yaxt="n",xlab="",ylab="",type="n")
  col_dots<-bluered[sapply(1:length(predo3),function(x) which.min(abs(seq(rango[1],rango[2],(1/999)*diff(rango))-predo3[x])))]
  invisible(sapply(1:length(predo3),function(x) polygon(c(grdi3[x,1]-by1_n,grdi3[x,1]+by1_n,
                                                          grdi3[x,1]+by1_n,grdi3[x,1]-by1_n),
                                                        c(grdi3[x,2]-by2_n,grdi3[x,2]-by2_n,
                                                          grdi3[x,2]+by2_n,grdi3[x,2]+by2_n),
                                                        border=NA,col=col_dots[x])))
  ncat=20
  k2d_out<-kde2d.weighted(grdi3[,1],
                          grdi3[,2], 
                          n = ncat, 
                          w=predo3)
  range_z<-range(k2d_out$z)
  pretty_0.004<-c(seq(0,0.996,0.004)[range_z[1]>seq(0,0.996,0.004)&range_z[1]<seq(0.004,1,0.004)],
                  seq(0.004,1,0.004)[range_z[2]<seq(0.004,1,0.004)&range_z[2]>seq(0,0.996,0.004)])
  contour(k2d_out, drawlabels=FALSE, levels=seq(pretty_0.004[1],pretty_0.004[2],0.004),add=TRUE,lwd=0.6,lty="dashed",col="white")
  
  text(mean(par("usr")[1:2]),
       par("usr")[4]-(0.15/par("pin")[2])*diff(par("usr")[3:4]),
       "G",font=2,cex=1.5,
       col="white")
  
  #Add median measures
  abline(v=median(pref_stat_BC$log_light_HC.z,na.rm = T),lty="dashed",
         col="yellow")
  abline(h=median(pref_stat_BC$log_light_TLC.z,na.rm = T),lty="dashed",
         col="yellow")
  
  mtext(substitute(paste("Non-Red BC to ",italic('H. t. linaresi'))),3,line=0.1,cex=0.65)
  
  yaxis_val.z<-as.numeric(scale(log10(xaxis_val*1000),attr(pref_stat_BC$log_light_TLC.z,"scaled:center"),attr(pref_stat_BC$log_light_TLC.z,"scaled:scale")))
  axis(2,at=yaxis_val.z,paste0(xaxis_val,"k"),line=-0.4,lwd=0,cex.axis=0.85,las=2)
  axis(2,at=yaxis_val.z,labels=F,tck=-0.03)
  
  xaxis_val.z<-as.numeric(scale(log10(xaxis_val*1000),attr(pref_stat_BC$log_light_HC.z,"scaled:center"),attr(pref_stat_BC$log_light_HC.z,"scaled:scale")))
  axis(1,at=xaxis_val.z,paste0(xaxis_val,"k"),line=-0.7,lwd=0,cex.axis=0.85)
  axis(1,at=xaxis_val.z,labels=F,tck=-0.03)
  
  
  
  par(mar=c(0,0.4,1.6,0.4))
  plot(1,1,ylim=c(min2_trans_n-by2_n,max2_trans_n+by2_n),
       xlim=c(min1_trans_n-by1_n,max1_trans_n+by1_n),xaxs="i",yaxs="i",xaxt="n",
       yaxt="n",xlab="",ylab="",type="n")
  col_dots<-bluered[sapply(1:length(predo4),function(x) which.min(abs(seq(rango[1],rango[2],(1/999)*diff(rango))-predo4[x])))]
  invisible(sapply(1:length(predo4),function(x) polygon(c(grdi4[x,1]-by1_n,grdi4[x,1]+by1_n,
                                                          grdi4[x,1]+by1_n,grdi4[x,1]-by1_n),
                                                        c(grdi4[x,2]-by2_n,grdi4[x,2]-by2_n,
                                                          grdi4[x,2]+by2_n,grdi4[x,2]+by2_n),
                                                        border=NA,col=col_dots[x])))
  ncat=20
  k2d_out<-kde2d.weighted(grdi4[,1],
                          grdi4[,2], 
                          n = ncat, 
                          w=predo4)
  range_z<-range(k2d_out$z)
  pretty_0.004<-c(seq(0,0.996,0.004)[range_z[1]>seq(0,0.996,0.004)&range_z[1]<seq(0.004,1,0.004)],
                  seq(0.004,1,0.004)[range_z[2]<seq(0.004,1,0.004)&range_z[2]>seq(0,0.996,0.004)])
  contour(k2d_out, drawlabels=FALSE, levels=seq(pretty_0.004[1],pretty_0.004[2],0.004),add=TRUE,lwd=0.6,lty="dashed",col="white")
  
  text(mean(par("usr")[1:2]),
       par("usr")[4]-(0.15/par("pin")[2])*diff(par("usr")[3:4]),
       "H",font=2,cex=1.5,
       col="white")
  
  #Add median measures
  abline(v=median(pref_stat_BC$log_light_HC.z,na.rm = T),lty="dashed",
         col="yellow")
  abline(h=median(pref_stat_BC$log_light_TLC.z,na.rm = T),lty="dashed",
         col="yellow")
  
  mtext(substitute(paste("Red BC to ",italic('H. t. linaresi'))),3,line=0.1,cex=0.65)
  
  xaxis_val.z<-as.numeric(scale(log10(xaxis_val*1000),attr(pref_stat_BC$log_light_HC.z,"scaled:center"),attr(pref_stat_BC$log_light_HC.z,"scaled:scale")))
  axis(1,at=xaxis_val.z,paste0(xaxis_val,"k"),line=-0.7,lwd=0,cex.axis=0.85)
  axis(1,at=xaxis_val.z,labels=F,tck=-0.03)
  
  
  #Now we draw the legend
  
  add_scale(col_range=col_range)
  
  mtext(substitute(paste("Illuminance at mounted ",italic('H. heurippa')," female [",italic('lux'),"]")),side=1,line=-1.15,outer=T,cex=0.65)
  
  #Add small empty plot
  old_mar<-par("mar")
  par(mar=rep(0,4))
  plot(1,1,bty="n",type="n",xaxt="n",yaxt="n",xlab="",ylab="")
  par(mar=old_mar)
  
}


#Function for plotting the raw light data with 2D Kernel densities
light_plot<-function(whole_dataset, #Dataset
                     subset_TF, #Subset criterion
                     ncat=10, #Number categories for Kernel densities
                     rango, #x- and y-range
                     add_x=T, #Whether to add x-axis
                     add_y=T, #Whether to add y-axis
                     axis_val=c(1.5,5,15,50,150)*1000, #axis values
                     xat=1, #Position of x-axis
                     yat=2, #Position of y-axis
                     letter="A" #Panel letter
){
  
  #We just randomly reorder the table so there is no "layers" of data
  reorderer<-sample(1:nrow(whole_dataset))
  whole_dataset<-whole_dataset[reorderer,]
  subset_TF<-subset_TF[reorderer]
  
  #We remove all rows where there is incomplete data
  na_remover<-!is.na(whole_dataset$log_light_HC)&!is.na(whole_dataset$log_light_TLC)
  whole_dataset<-whole_dataset[na_remover,]
  subset_TF<-subset_TF[na_remover]
  
  #We subset the table
  whole_dataset<-whole_dataset[subset_TF,]
  
  #If range was not passed, calculate here
  if(missing(rango)){
    rango<-range(c(whole_dataset$log_light_HC,
                   whole_dataset$log_light_TLC),na.rm=T)
  }
  
  #Plot empty plot
  plot(1,1,
       xlim=c(rango[1]-0.03*diff(rango),rango[2]+0.03*diff(rango)),
       ylim=c(rango[1]-0.03*diff(rango),rango[2]+0.03*diff(rango)),
       xaxs="i",yaxs="i",
       type="n",xaxt="n",yaxt="n",xlab="",ylab="")
  
  #Add axes if wished
  if(add_x){
    axis(xat,at=log10(axis_val),tck=-0.03,labels=F)
    axis(xat,at=log10(axis_val),paste0(axis_val/1000,"k"),lwd=0,line=-0.7,cex.axis=0.85)
  }
  if(add_y){
    axis(yat,at=log10(axis_val),tck=-0.03,labels=F)
    axis(yat,at=log10(axis_val),paste0(axis_val/1000,"k"),las=2,lwd=0,line=-0.4,cex.axis=0.85)
  }
  
  #Plot raw data points
  points(whole_dataset$log_light_HC,
         whole_dataset$log_light_TLC,
         pch=21,bg=ifelse(whole_dataset$court_lin_heu_01==1,
                          adjustcolor("red",0.1),adjustcolor("blue",0.15)),
         col=NA)
  
  #Kernel density for responses to lin
  kerno_blu<-kde2d(whole_dataset$log_light_HC[whole_dataset$court_lin_heu_01==0],
                   whole_dataset$log_light_TLC[whole_dataset$court_lin_heu_01==0],
                   n = 1000)
  #Kernel density for responses to heu
  kerno_red<-kde2d(whole_dataset$log_light_HC[whole_dataset$court_lin_heu_01==1],
                   whole_dataset$log_light_TLC[whole_dataset$court_lin_heu_01==1],
                   n = 1000)
  
  #Get range of Kernel
  range_z<-range(kerno_red$z,kerno_blu$z)
  #Define levels of mountain ridges
  interval<-range_z[2]/(ncat-1)
  
  #Define number of ridges for each Kernel, whereas step size is determined by maximum value from both Kernels
  pretty_interval<-c(seq(0,range_z[2]-interval,interval)[range_z[1]>seq(0,range_z[2]-interval,interval)&range_z[1]<seq(interval,range_z[2],interval)],
                     seq(interval,range_z[2],interval)[range_z[2]<seq(interval,range_z[2],interval)&range_z[2]>seq(0,range_z[2]-interval,interval)])
  pretty_interval<-c(0,range_z[2])
  
  #Draw the contours of the Kernels
  contour(kerno_red,drawlabels=FALSE,add=TRUE,lwd=1,col="red",
          levels=seq(pretty_interval[1],pretty_interval[2],interval),lty="dashed")
  contour(kerno_blu,drawlabels=FALSE,add=TRUE,lwd=1,col="blue",
          levels=seq(pretty_interval[1],pretty_interval[2],interval),lty="dashed")
  
  #Add panel letter
  text(par("usr")[1]+(0.2/par("pin")[1])*diff(par("usr")[1:2]),
       par("usr")[4]-(0.15/par("pin")[2])*diff(par("usr")[3:4]),
       labels=letter,font=2,cex=1.5,
       pos=2,offset=0)
  
}


#Function for plotting the local preferences
light_plot_pref<-function(whole_dataset, #Dataset
                          subset_TF, #Subset criterion
                          number_bins, #Number of BINs
                          rango, #Value range
                          add_x=T, #Whether to add x-axis 
                          add_y=T, #Whether to add y-axis
                          axis_val=c(1.5,5,15,50,150)*1000, #axis values
                          transparency=T, #Whether transparency should be applied according to number of samples
                          transp_scale_log=T, #Whether sample sizes should be log-scaled for transparency scaling
                          xat=1, #Position of x-axis
                          yat=2, #Position of y-axis
                          letter="A" #Panel letter
){
  
  #We just randomly reorder the table so there is no "layers" of data
  reorderer<-sample(1:nrow(whole_dataset))
  whole_dataset<-whole_dataset[reorderer,]
  subset_TF<-subset_TF[reorderer]
  
  #We remove all rows where there is incomplete data
  na_remover<-!is.na(whole_dataset$log_light_HC)&!is.na(whole_dataset$log_light_TLC)
  whole_dataset<-whole_dataset[na_remover,]
  subset_TF<-subset_TF[na_remover]
  
  #We subset the table
  whole_dataset<-whole_dataset[subset_TF,]
  
  #Calculate range if missing argument
  if(missing(rango)){
    rango<-range(c(whole_dataset$log_light_HC,
                   whole_dataset$log_light_TLC),na.rm=T)
  }
  
  #Get the BIN boundaries. Set one very high boundary (necessary for code to work on last actual BIN)
  bins<-c(seq(rango[1],rango[2],length.out=number_bins),1000)
  
  #Calculate preferences per BIN
  pref_bin<-sapply(1:number_bins,function(x) sapply(1:number_bins,function(y) sum(whole_dataset$court_lin_heu_01[whole_dataset$log_light_HC>=bins[x]&whole_dataset$log_light_HC<bins[x+1]&whole_dataset$log_light_TLC>=bins[y]&whole_dataset$log_light_TLC<bins[y+1]])/sum(whole_dataset$log_light_HC>=bins[x]&whole_dataset$log_light_HC<bins[x+1]&whole_dataset$log_light_TLC>=bins[y]&whole_dataset$log_light_TLC<bins[y+1])))
  #Number of observations per BIN
  num_obs<-sapply(1:number_bins,function(x) sapply(1:number_bins,function(y) sum(whole_dataset$log_light_HC>=bins[x]&whole_dataset$log_light_HC<bins[x+1]&whole_dataset$log_light_TLC>=bins[y]&whole_dataset$log_light_TLC<bins[y+1])))
  
  #Open empty plot
  plot(1,1,
       xlim=c(rango[1]-0.03*diff(rango),rango[2]+0.03*diff(rango)),
       ylim=c(rango[1]-0.03*diff(rango),rango[2]+0.03*diff(rango)),
       xaxs="i",yaxs="i",
       type="n",xaxt="n",yaxt="n",xlab="",ylab="")
  
  #Define range for colour palette
  col_rang<-0:1
  #Define colour ramp
  paletto<-colorRampPalette(c("blue", "red"))
  #Sample 1000 colours
  bluered<-paletto(1000)
  
  #Get colour of each square from the palette
  #Depending on whether we want transparency of the squares, this is slightly modified
  if(transparency){
    if(transp_scale_log){
      col_mat<-sapply(1:number_bins,function(x) sapply(1:number_bins,function(y) ifelse(num_obs[x,y]==0,"000000",adjustcolor(bluered[which.min(abs(seq(col_rang[1],col_rang[2],(1/1000)*diff(col_rang))-pref_bin[x,y]))],log(num_obs[x,y])/max(log(num_obs))))))
    } else{
      col_mat<-sapply(1:number_bins,function(x) sapply(1:number_bins,function(y) ifelse(num_obs[x,y]==0,"000000",adjustcolor(bluered[which.min(abs(seq(col_rang[1],col_rang[2],(1/1000)*diff(col_rang))-pref_bin[x,y]))],num_obs[x,y]/max(num_obs)))))
    }
  } else{
    col_mat<-sapply(1:number_bins,function(x) sapply(1:number_bins,function(y) ifelse(num_obs[x,y]==0,"000000",bluered[which.min(abs(seq(col_rang[1],col_rang[2],length.out = 1000)-pref_bin[x,y]))])))
  }
  
  #Get boundaries for each BIN square
  bins_lines<-c(par("usr")[1],sapply(2:number_bins,function(x) mean(c(bins[x-1],bins[x]))),par("usr")[2])
  
  #Add colourful squares!
  invisible(sapply(1:number_bins,function(x) sapply(1:number_bins,function(y) polygon(c(bins_lines[x],bins_lines[x+1],
                                                                                        bins_lines[x+1],bins_lines[x]),
                                                                                      c(bins_lines[y],bins_lines[y],
                                                                                        bins_lines[y+1],bins_lines[y+1]),
                                                                                      border=NA,col=col_mat[x,y]))))
  #Add again the plot box (may have been covered at some places)
  box()
  
  #If wished, add axes
  if(add_x){
    axis(xat,at=log10(axis_val),tck=-0.03,labels=F)
    axis(xat,at=log10(axis_val),paste0(axis_val/1000,"k"),lwd=0,line=-0.7,cex.axis=0.85)
  }
  if(add_y){
    axis(yat,at=log10(axis_val),tck=-0.03,labels=F)
    axis(yat,at=log10(axis_val),paste0(axis_val/1000,"k"),las=2,lwd=0,line=-0.4,cex.axis=0.85)
  }
  
  #Add panel letter
  text(par("usr")[1]+(0.2/par("pin")[1])*diff(par("usr")[1:2]),
       par("usr")[4]-(0.15/par("pin")[2])*diff(par("usr")[3:4]),
       labels=letter,font=2,cex=1.5,
       pos=2,offset=0)
  
  
}


####### TETRAD EXPERIMENTS

#Get histogram for posteriors
hist_no_plot2<-function(vec,br_seq=seq(-1,1,length.out=1000)){
  no_plot<-hist(vec,breaks=br_seq,plot=F)
  return(no_plot)
}

#Add posterior
add_polygon2<-function(histo,y_ext,num_br){
  invisible(sapply(1:(num_br-1),function(x) polygon(c(histo$breaks[x],histo$breaks[x+1],rev(c(histo$breaks[x],histo$breaks[x+1]))),
                                                    c(rep(y_ext,2),y_ext+rep(histo$counts[x],2)),
                                                    col="grey",border=NA)))
}

#Highlight 95% CrI in posterior
add_polygon2_red<-function(quanto,histo,y_ext,num_br){
  polygon(c(quanto[1],quanto[1],rep(histo$breaks[quanto[1]<histo$breaks&quanto[3]>histo$breaks],each=2),quanto[3],quanto[3],
            rev(c(quanto[1],quanto[1],rep(histo$breaks[quanto[1]<histo$breaks&quanto[3]>histo$breaks],each=2),quanto[3],quanto[3]))),
          y_ext+c(0,rep(histo$counts[quanto[1]>histo$breaks[1:(num_br-1)]&quanto[1]<histo$breaks[2:num_br]],2),
                  rep(histo$counts[quanto[1]<histo$breaks&quanto[3]>histo$breaks],each=2),0,
                  rep(0,sum(quanto[1]<histo$breaks&quanto[3]>histo$breaks)*2+4)),
          col=adjustcolor("red",0.3),border=NA)
}

#Put a coloured line at the 50% quantile
fifty_quant<-function(quanto,histo,y_ext,num_br){
  segments(quanto[2],y_ext+0,quanto[2],y_ext+histo$counts[quanto[2]>histo$breaks[1:(num_br-1)]&quanto[2]<histo$breaks[2:num_br]],
           lwd=2.5,col="orange",lend=3)
}

#Paste the combination of mating
venus_mars<-function(male,female,y_mult,y_mult_male,y_mult_female,mult_fact,
                     right_x_lim,move_N_right,cex_N,space_spec){
  text(right_x_lim-space_spec*right_x_lim+move_N_right*right_x_lim,y_mult*mult_fact,"\\VE",vfont=c("sans serif symbol","plain"),cex=1.8*cex_N)
  text(right_x_lim+move_N_right*right_x_lim,y_mult*mult_fact,"\\MA",vfont=c("sans serif symbol","plain"),cex=1.8*cex_N)
  text(right_x_lim-space_spec*right_x_lim+move_N_right*right_x_lim,y_mult_female*mult_fact,substitute(paste(italic(b)),list(b=c("H. t. linaresi","H. heurippa")[female==c("lin","heu")])),cex=0.6*cex_N)
  text(right_x_lim+move_N_right*right_x_lim,y_mult_male*mult_fact,substitute(paste(italic(b)),list(b=c("H. t. linaresi","H. heurippa")[male==c("lin","heu")])),cex=0.6*cex_N)
}


#Bigger plotting command for tetrad figure.
tetrad_fig<-function(prop_heu_lin,
                     prop_lin_lin,
                     prop_heu_heu,
                     prop_lin_heu,
                     quant_heu_lin,
                     quant_lin_lin,
                     quant_heu_heu,
                     quant_lin_heu,
                     datasheet,
                     bty_set="n",
                     mar_add_left_bottom=0,
                     frame_type="n",
                     move_N_right=0.04,
                     cex_N=1,
                     space_spec=0.18,
                     curr_region="",
                     estimations){
  
  #Proportion of each type of mating pair.
  
  #Do a histogram of the posterior for each mating pair. Don't plot yet.
  hist_heu_lin<-hist_no_plot2(prop_heu_lin)
  hist_lin_lin<-hist_no_plot2(prop_lin_lin)
  hist_heu_heu<-hist_no_plot2(prop_heu_heu)
  hist_lin_heu<-hist_no_plot2(prop_lin_heu)
  
  #Get maximum histogram count and multiply by factor. This will be the extend of each plot except the uppermost
  maximum_counts<-c(max(hist_heu_lin$counts),max(hist_lin_lin$counts),max(hist_heu_heu$counts),max(hist_lin_heu$counts))
  max_count_hist<-1.2*max(maximum_counts)
  
  #Get number of breaks of any histogram
  num_br<-length(hist_heu_lin$breaks)
  
  #Get minimum and maximum x values
  left_x_lim<-0
  right_x_lim<-0.7
  
  #Open empty plot
  par(mar=c(3.5+mar_add_left_bottom,0.5,0,0.5))
  plot(1,1,type="n",bty=bty_set,yaxt="n",xaxt="n",
       ylim=c(-0.07*max_count_hist,4*max_count_hist),xlim=c(left_x_lim,0.74),main="",
       ylab="",xlab="",yaxs="i")
  
  par(xpd=T)
  
  #Add heu_lin posterior distribution at top of graph
  add_polygon2(histo=hist_heu_lin,y_ext=3*max_count_hist,num_br=num_br)
  #Shade credibility interval red.
  add_polygon2_red(quanto=quant_heu_lin,histo=hist_heu_lin,y_ext=3*max_count_hist,num_br=num_br)
  #Add 50% quantile
  fifty_quant(quanto=quant_heu_lin,histo=hist_heu_lin,y_ext=3*max_count_hist,num_br=num_br)
  
  #Repeat the same for all other regions
  add_polygon2(histo=hist_lin_lin,y_ext=2*max_count_hist,num_br=num_br)
  add_polygon2_red(quanto=quant_lin_lin,histo=hist_lin_lin,y_ext=2*max_count_hist,num_br=num_br)
  fifty_quant(quanto=quant_lin_lin,histo=hist_lin_lin,y_ext=2*max_count_hist,num_br=num_br)
  
  add_polygon2(histo=hist_heu_heu,y_ext=1*max_count_hist,num_br=num_br)
  add_polygon2_red(quanto=quant_heu_heu,histo=hist_heu_heu,y_ext=1*max_count_hist,num_br=num_br)
  fifty_quant(quanto=quant_heu_heu,histo=hist_heu_heu,y_ext=1*max_count_hist,num_br=num_br)
  
  add_polygon2(histo=hist_lin_heu,y_ext=0*max_count_hist,num_br=num_br)
  add_polygon2_red(quanto=quant_lin_heu,histo=hist_lin_heu,y_ext=0*max_count_hist,num_br=num_br)
  fifty_quant(quanto=quant_lin_heu,histo=hist_lin_heu,y_ext=0*max_count_hist,num_br=num_br)
  
  #Add sample size for each pair and paste the combination of mating
  venus_mars(male="lin",female="heu",y_mult=3.6,y_mult_male=3.4,y_mult_female=3.39,mult_fact=max_count_hist,
             right_x_lim=right_x_lim,move_N_right=move_N_right,cex_N=cex_N,space_spec=space_spec)
  text(right_x_lim-0.5*right_x_lim*space_spec+move_N_right*right_x_lim,3.2*max_count_hist,paste0("N = ",sum(datasheet$counts[datasheet$combo=="heu_lin"])),cex=0.7*cex_N)
  venus_mars(male="lin",female="lin",y_mult=2.6,y_mult_male=2.4,y_mult_female=2.4,mult_fact=max_count_hist,
             right_x_lim=right_x_lim,move_N_right=move_N_right,cex_N=cex_N,space_spec=space_spec)
  text(right_x_lim-0.5*right_x_lim*space_spec+move_N_right*right_x_lim,2.2*max_count_hist,paste0("N = ",sum(datasheet$counts[datasheet$combo=="lin_lin"])),cex=0.7*cex_N)
  venus_mars(male="heu",female="heu",y_mult=1.6,y_mult_male=1.39,y_mult_female=1.39,mult_fact=max_count_hist,
             right_x_lim=right_x_lim,move_N_right=move_N_right,cex_N=cex_N,space_spec=space_spec)
  text(right_x_lim-0.5*right_x_lim*space_spec+move_N_right*right_x_lim,1.2*max_count_hist,paste0("N = ",sum(datasheet$counts[datasheet$combo=="heu_heu"])),cex=0.7*cex_N)
  venus_mars(male="heu",female="lin",y_mult=0.6,y_mult_male=0.39,y_mult_female=0.4,mult_fact=max_count_hist,
             right_x_lim=right_x_lim,move_N_right=move_N_right,cex_N=cex_N,space_spec=space_spec)
  text(right_x_lim-0.5*right_x_lim*space_spec+move_N_right*right_x_lim,0.2*max_count_hist,paste0("N = ",sum(datasheet$counts[datasheet$combo=="lin_heu"])),cex=0.7*cex_N)
  
  #Add line dividing lin and heu males
  abline(h=max_count_hist+(1.02/1.1)*max_count_hist,lwd=0.5)
  
  #Draw frame
  if(grepl("top",frame_type)){
    abline(h=par("usr")[3]-((par("mai")[1]/par("pin")[2]))*diff(par("usr")[3:4]),lwd=4)
  }
  
  par(xpd=F)
  
  #Add estimates
  segments(estimations[2,],((3:0)+0.07)*max_count_hist+maximum_counts,
           estimations[3,],((3:0)+0.07)*max_count_hist+maximum_counts,
           col="purple",lwd=1.5,lend=3)
  points(estimations[1,],((3:0)+0.07)*max_count_hist+maximum_counts,pch=20,cex=1.1,col="purple")

  #Add line at 0.25 (all pairs equally probable)
  abline(v=0.25,lwd=2,lty="dashed")
  
  #Axis and axis title
  axis(1,at=c(0,0.25,0.5,0.75,1),cex.axis=0.8,line=-0.5,lwd=0)
  axis(1,at=c(0,0.25,0.5,0.75,1),labels=F,tck=-0.02)
  mtext("Proportion Mating Pair",1,line=2.3,cex=1)
  
}
