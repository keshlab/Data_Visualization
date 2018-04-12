## requires returnAdj and get_correlations

## subset data based on group
pro<-dat[(dat$SubjectType=='Proband'),]
sz<-dat[dat$DXGROUP_2=='SZP' ,]
sza<-dat[dat$DXGROUP_2=='SADP' ,]
bpp<-dat[dat$DXGROUP_2=='BPP' ,]
nc<-dat[dat$DXGROUP_2=='NC' ,]
bt1<-btg[btg$BIOTYPEGROUP_1=='BT1' ,]
bt2<-btg[btg$BIOTYPEGROUP_1=='BT2' ,]
bt3<-btg[btg$BIOTYPEGROUP_1=='BT3' ,]

# list of covariates
covar_vol<-c('Age_cal','site','sex','RACE','IntraCranialVol_x')

# run correlations within group of interest
pro_cort<-get_correlations(pro, "choroid_plexus", cort, covar_vol, covar_vol)
pro_cort$Group<-'Proband'
sz_cort<-get_correlations(sz, "choroid_plexus", cort, covar_vol, covar_vol)
sz_cort$Group<-'SZ'
sza_cort<-get_correlations(sza, "choroid_plexus", cort, covar_vol, covar_vol)
sza_cort$Group<-'SZA'
bp_cort<-get_correlations(bpp, "choroid_plexus", cort, covar_vol, covar_vol)
bp_cort$Group<-'BPP'
bt1_cort<-get_correlations(bt1, "choroid_plexus", cort, covar_vol, covar_vol)
bt1_cort$Group<-'BT1'
bt2_cort<-get_correlations(bt2, "choroid_plexus", cort, covar_vol, covar_vol)
bt2_cort$Group<-'BT2'
bt3_cort<-get_correlations(bt3, "choroid_plexus", cort, covar_vol, covar_vol)
bt3_cort$Group<-'BT3'
nc_cort<-get_correlations(nc,"choroid_plexus", cort, covar_vol, covar_vol) 
nc_cort$Group<-'NC'

## stack data together
imag_corr<-rbind(pro_cort, sz_cort, sza_cort, bp_cort, bt1_cort, bt2_cort, bt3_cort, nc_cort)
imag_corr$Group<-factor(imag_corr$Group)
imag_corr$Group<-factor(imag_corr$Group, levels = unique(imag_corr$Group))

## create column of stars based on p vals
imag_corr$stars <- cut(imag_corr$adjusted_p_value, breaks=c(-Inf, 0.001, 0.01, 0.05, Inf), 
                       label=c("***", "**", "*", ""))  # Create column of significance labels

# extract Region of interest from combo column
imag_corr$Region<-sapply(str_split(imag_corr$combo,'-'), function(x) x[2])

# plot
plot_imag<-ggplot(imag_corr, aes(Group, Region))
plot_imag+geom_tile(data=imag_corr, aes(fill=r_value), color="white")+geom_text(aes(label = round(imag_corr$r_value, 2)), size=8)+
  geom_text(aes(label=stars), color="black", size=9,vjust=-0.2)+ylab('')+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-.65,.65), space = "Lab", name="Correlation\nCoefficient")+
  theme(legend.title = element_text(size=16),legend.text=element_text(size=14),axis.text.x=element_text(size = 18),
        axis.text.y=element_text(size = 18),title=element_text(size=18), plot.title = element_text(hjust = 0.5))+
  scale_y_discrete(labels=c('TotalGrayVol'='Total\nGray\nVolume','Lat_Ventricle'='Lateral\nVentricle\nVolume',
                            'CorticalWhiteMatterVol_x'='Total\nWhite\nVolume'))

