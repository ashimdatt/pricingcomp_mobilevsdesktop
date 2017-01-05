 getwd()

library(sqldf)
library(ggplot2)
library(data.table)
library(gridExtra)

library("reshape2")
#data load
adconv_cleaned <- fread("ma_adconv_product_us_cleaned_2",header=T, sep="\t",integer64="character") 
adconv_sub<-sqldf("select userid,event_type_str,vertical_name,sub_vertical_name,                                                     
                  ultimate_parent_name,country,age_bracket,usd_amount,conversion_device,ds,ma_sitename,ma_category,ma_subcategory 
                  from adconv_cleaned")
adconv_sub<-transform(adconv_sub,ds=as.Date(adconv_sub$ds))
adconv_sub<-adconv_sub[order(adconv_sub$ds),]
str(adconv_sub)
adconv_sub$chardt<-as.character(adconv_sub$ds)

#Ratio of amount spent on mobile vs web 

#any number of purchases

adconv_sub$conversion_device_new <- ifelse(adconv_sub$conversion_device =="Desktop", "Desktop", 
                                           (ifelse(adconv_sub$conversion_device =="iPad","Tablet",(ifelse(adconv_sub$conversion_device =="Androod Tablet","Tablet","Mobile")))))

adconv_sub_agg_any_overall<-aggregate(list(usd_amount=adconv_sub$usd_amount), 
                                      by=list(conversion_device_new=adconv_sub$conversion_device_new), median)
adconv_sub_agg_any_mobile_desktop_ratio<-adconv_sub_agg_any_overall[which(adconv_sub_agg_any_overall$conversion_device_new=='Mobile'),"usd_amount"]/adconv_sub_agg_any_overall[which(adconv_sub_agg_any_overall$conversion_device_new=='Desktop'),"usd_amount"]
adconv_sub_agg_any_tablet_desktop_ratio<-adconv_sub_agg_any_overall[which(adconv_sub_agg_any_overall$conversion_device_new=='Tablet'),"usd_amount"]/adconv_sub_agg_any_overall[which(adconv_sub_agg_any_overall$conversion_device_new=='Desktop'),"usd_amount"]

adconv_sub_new<-sqldf("select * from adconv_sub where chardt>'2015-03-01'")
# by dates

adconv_sub_agg_any<-aggregate(list(usd_amount=adconv_sub_new$usd_amount), 
                              by=list(ds=adconv_sub_new$ds,conversion_device_new=adconv_sub_new$conversion_device_new), median)
adconv_device_any<-sqldf("select ds,
                         sum(case when conversion_device_new ='Desktop' then usd_amount else 0 end)/100 as Desktop_median,
                         sum(case when conversion_device_new ='Mobile' then usd_amount else 0 end)/100 as Mobile_median,
                         sum(case when conversion_device_new='Tablet' then usd_amount else 0 end)/100 as Tablet_median
                         from adconv_sub_agg_any group by ds")
adconv_device_any$ratio_phone<-adconv_device_any$Mobile_median/adconv_device_any$Desktop_median
adconv_device_any$ratio_tablet<-adconv_device_any$Tablet_median/adconv_device_any$Desktop_median
adconv_device_any_plot<-adconv_device_any[,c(1,5,6)]
adconv_device_any_plot <- melt(adconv_device_any_plot, id="ds")

# 2 buys atleast


# str(adconv_sub)
#adconv_sub_buyers<-sqldf("select userid, count(1) as num_buys from adconv_sub group by userid" )
#str(adconv_sub_buyers) 

#adconv_sub_buyers_2more<-sqldf("select userid from adconv_sub_buyers where num_buys>=2")

#adconv_sub<-sqldf("select a.* from adconv_sub a join adconv_sub_buyers_2more b
#                 on a.userid=b.userid")
#by dates
#adconv_sub_agg<-aggregate(list(usd_amount=adconv_sub$usd_amount), 
#                         by=list(ds=adconv_sub$ds,conversion_device_new=adconv_sub$conversion_device_new), median)

#str(adconv_sub_agg)

#adconv_device<-sqldf("select ds,
#                    sum(case when conversion_device_new ='Desktop' then usd_amount else 0 end)/100 as Desktop_median,
#                   sum(case when conversion_device_new ='Mobile' then usd_amount else 0 end)/100 as Mobile_median
#                  from adconv_sub_agg group by ds")
#adconv_device$ratio<-adconv_device$Mobile_median/adconv_device$Desktop_median

#bought on 2 devices overall
adconv_mobile_buyers_overall<-sqldf("select * from adconv_sub where conversion_device_new in('Mobile','Tablet')")

adconv_desktop_buyers_overall<-sqldf("select * from adconv_sub where conversion_device_new='Desktop' ")

advonv_mobile_desktop_buyers_overall<-sqldf("select a.* from adconv_mobile_buyers_overall a join adconv_desktop_buyers_overall b on a.userid=b.userid")
advonv_mobile_desktop_buyers_overall<-sqldf("select a.* from adconv_sub a join advonv_mobile_desktop_buyers_overall b on a.userid=b.userid")


advonv_mobile_desktop_buyers_overall_new<-sqldf("select * from advonv_mobile_desktop_buyers_overall where chardt>'2015-03-01'")

adconv_sub_mobile_desktop_overall_agg<-aggregate(list(usd_amount=advonv_mobile_desktop_buyers_overall$usd_amount), 
                                                 by=list(conversion_device_new=advonv_mobile_desktop_buyers_overall$conversion_device_new),
                                                 median)

str(adconv_sub_mobile_desktop_overall_agg)

adconv_sub_mobile_desktop_overall_ratio<-adconv_sub_mobile_desktop_overall_agg[which(adconv_sub_mobile_desktop_overall_agg$conversion_device_new=='Mobile'),"usd_amount"]/adconv_sub_mobile_desktop_overall_agg[which(adconv_sub_mobile_desktop_overall_agg$conversion_device_new=='Desktop'),"usd_amount"]

adconv_sub_mobile_desktop_overall_tablet_ratio<-adconv_sub_mobile_desktop_overall_agg[which(adconv_sub_mobile_desktop_overall_agg$conversion_device_new=='Tablet'),"usd_amount"]/adconv_sub_mobile_desktop_overall_agg[which(adconv_sub_mobile_desktop_overall_agg$conversion_device_new=='Desktop'),"usd_amount"]

#by dates


adconv_sub_mobile_desktop_overall_by_date<-aggregate(list(usd_amount=advonv_mobile_desktop_buyers_overall_new$usd_amount), 
                                                     by=list(ds=advonv_mobile_desktop_buyers_overall_new$ds,conversion_device_new=advonv_mobile_desktop_buyers_overall_new$conversion_device_new), median)


adconv_device_mobile_desktop_overall_date<-sqldf("select ds,
                                                 sum(case when conversion_device_new ='Desktop' then usd_amount else 0 end)/100 as Desktop_median,
                                                 sum(case when conversion_device_new ='Mobile' then usd_amount else 0 end)/100 as Mobile_median,
                                                 sum(case when conversion_device_new ='Tablet' then usd_amount else 0 end)/100 as Tablet_median
                                                 from adconv_sub_mobile_desktop_overall_by_date group by ds")
adconv_device_mobile_desktop_overall_date$ratio_mobile<-adconv_device_mobile_desktop_overall_date$Mobile_median/
  adconv_device_mobile_desktop_overall_date$Desktop_median
adconv_device_mobile_desktop_overall_date$tablet_ratio<-adconv_device_mobile_desktop_overall_date$Tablet_median/
  adconv_device_mobile_desktop_overall_date$Desktop_median

adconv_device_mobile_desktop_overall_date_plot<-adconv_device_mobile_desktop_overall_date[,c(1,5,6)]
adconv_device_mobile_desktop_overall_date_plot <- melt(adconv_device_mobile_desktop_overall_date_plot, id="ds")

#overall
adconv_sub_agg_overall<-aggregate(list(usd_amount=adconv_sub$usd_amount), 
                                  by=list(conversion_device_new=adconv_sub$conversion_device_new), median)
adconv_sub_agg_overall_ratio<-adconv_sub_agg_overall[which(adconv_sub_agg_overall$conversion_device_new=='Mobile'),"usd_amount"]/adconv_sub_agg_overall[which(adconv_sub_agg_overall$conversion_device_new=='Desktop'),"usd_amount"]
adconv_sub_agg_overall_tablet_ratio<-adconv_sub_agg_overall[which(adconv_sub_agg_overall$conversion_device_new=='Tablet'),"usd_amount"]/adconv_sub_agg_overall[which(adconv_sub_agg_overall$conversion_device_new=='Desktop'),"usd_amount"]
str(adconv_sub_agg_overall_ratio)

adconv_sub_agg_plot<-data.frame(purchase_type=
                                  c("All purchases-mobile_vs_desktop","All purchases-tablet_vs_desktop",
                                    "People who bought on a mobile device (phone+tablet) and desktop- mobile_vs_desktop","People who bought on a mobile device (phone+tablet) and desktop- tablet_vs_desktop" ),
                                ratio=c(adconv_sub_agg_overall_ratio,adconv_sub_agg_overall_tablet_ratio,
                                        adconv_sub_mobile_desktop_overall_ratio, adconv_sub_mobile_desktop_overall_tablet_ratio))



#--msite vs apps

adconv_sub$interface<-ifelse(adconv_sub$conversion_device_new =="Desktop", "Desktop", 
                            (ifelse(adconv_sub$event_type_str =="app_custom_event.fb_mobile_purchase","Mobile app","Msite")))

adconv_sub_agg_any_overall_msite<-aggregate(list(usd_amount=adconv_sub$usd_amount), 
                                      by=list(interface=adconv_sub$interface), median)

adconv_sub_agg_any_msite_desktop_ratio<-adconv_sub_agg_any_overall_msite[which(adconv_sub_agg_any_overall_msite$interface=='Msite'),"usd_amount"]/adconv_sub_agg_any_overall_msite[which(adconv_sub_agg_any_overall_msite$interface=='Desktop'),"usd_amount"]
adconv_sub_agg_any_mobile_App_desktop_ratio<-adconv_sub_agg_any_overall_msite[which(adconv_sub_agg_any_overall_msite$interface=='Mobile app'),"usd_amount"]/adconv_sub_agg_any_overall_msite[which(adconv_sub_agg_any_overall_msite$interface=='Desktop'),"usd_amount"]

adconv_sub_agg_plot_msite<-data.frame(purchase_type=c("All purchases-msite_vs_desktop","All purchases-app_vs_desktop"),
                                ratio=c(adconv_sub_agg_any_msite_desktop_ratio,adconv_sub_agg_any_mobile_App_desktop_ratio))


#frequency- msite vs apps

adconv_sub_msite_app<-sqldf("select * from adconv_sub where interface in ('Mobile app','Msite')")

adconv_sub_msite_app_people<-sqldf("select userid, count(1) as freq from adconv_sub_msite_app group by userid ")

adconv_sub_msite_app_people$freq_bracket<-ifelse(adconv_sub_msite_app_people$freq>0 & adconv_sub_msite_app_people$freq<2,"1",
                                               (ifelse(adconv_sub_msite_app_people$freq>1 & adconv_sub_msite_app_people$freq<4,"2-3",
                                                       (ifelse(adconv_sub_msite_app_people$freq>3 & adconv_sub_msite_app_people$freq<7,"4-6","7+")))))
adconv_sub_msite_app_freq_advertiser<-sqldf("select ultimate_parent_name, interface, count(1) as num_purchase from adconv_sub_msite_app group by ultimate_parent_name,interface")
adconv_sub_msite_app_freq_advertiser_2<-sqldf("select ultimate_parent_name from adconv_sub_msite_app_freq_advertiser where num_purchase>5 and interface='Mobile app'")

adconv_sub_msite_app_freq_people<-sqldf("select a.freq_bracket,sum(case when b.interface='Mobile app' then 1 else 0 end) as Mobile_app,
                                        sum(case when b.interface='Msite' then 1 else 0 end) as Msite 
                                        from adconv_sub_msite_app b join adconv_sub_msite_app_people a
                                        on a.userid=b.userid
                                        join adconv_sub_msite_app_freq_advertiser_2 c on c.ultimate_parent_name=b.ultimate_parent_name group by freq_bracket ")

adconv_sub_msite_app_freq_people$Mobile_app_percent<-adconv_sub_msite_app_freq_people$Mobile_app/(adconv_sub_msite_app_freq_people$Mobile_app+adconv_sub_msite_app_freq_people$Msite)
adconv_sub_msite_app_freq_people$Msite_percent<-adconv_sub_msite_app_freq_people$Msite/(adconv_sub_msite_app_freq_people$Mobile_app+adconv_sub_msite_app_freq_people$Msite)





# ------------------------------ Time period analysis ----------------






#During Holidays

#All purchases
advonv_buyers_holidays<-sqldf("select * from adconv_sub where chardt>='2014-11-01' 
                              and chardt<='2014-12-31' ")
adconv_sub_holidays_agg<-aggregate(list(usd_amount=advonv_buyers_holidays$usd_amount), 
                                   by=list(age_bracket=advonv_buyers_holidays$age_bracket,conversion_device_new=advonv_buyers_holidays$conversion_device_new),
                                   median)

str(advonv_buyers_holidays)

adconv_holidays<-sqldf("select age_bracket,
                       sum(case when conversion_device_new ='Desktop' then usd_amount else 0 end)/100 as Desktop_median,
                       sum(case when conversion_device_new ='Mobile' then usd_amount else 0 end)/100 as Mobile_median,
                      sum(case when conversion_device_new ='Tablet' then usd_amount else 0 end)/100 as Tablet_median
                       from advonv_buyers_holidays group by age_bracket")
str(adconv_holidays)

adconv_holidays$ratio<-adconv_holidays$Mobile_median/adconv_holidays$Desktop_median
adconv_holidays$ratio_tablet<-adconv_holidays$Tablet_median/adconv_holidays$Desktop_median
adconv_holidays_plot<-adconv_holidays[,c(1,5,6)]
adconv_holidays_plot <- melt(adconv_holidays_plot, id="age_bracket")

#People who bought on both the devices
adconv_mobile_buyers_holidays<-sqldf("select * from adconv_sub where conversion_device_new in ('Mobile','Tablet') and chardt>='2014-11-01' 
                                     and chardt<='2014-12-31'")

adconv_desktop_buyers_holidays<-sqldf("select * from adconv_sub where conversion_device_new='Desktop'and chardt>='2014-11-01'
                                      and chardt<='2014-12-31'")

advonv_mobile_desktop_buyers_holidays<-sqldf("select a.* from adconv_mobile_buyers_holidays a join adconv_desktop_buyers_holidays b on a.userid=b.userid")
advonv_mobile_desktop_buyers_holidays<-sqldf("select a.* from adconv_sub a join advonv_mobile_desktop_buyers_holidays b on a.userid=b.userid and 
                                             a.chardt>='2014-11-01'and a.chardt<='2014-12-31'")

str(advonv_mobile_desktop_buyers_holidays)
adconv_sub_mobile_desktop_holidays_agg<-aggregate(list(usd_amount=advonv_mobile_desktop_buyers_holidays$usd_amount), 
                                                  by=list(age_bracket=advonv_mobile_desktop_buyers_holidays$age_bracket,conversion_device_new=advonv_mobile_desktop_buyers_holidays$conversion_device_new),
                                                  median)

str(adconv_sub_mobile_desktop_holidays_agg)

adconv_mobile_desktop_holidays<-sqldf("select age_bracket,
                                      sum(case when conversion_device_new ='Desktop' then usd_amount else 0 end)/100 as Desktop_median,
                                      sum(case when conversion_device_new ='Mobile' then usd_amount else 0 end)/100 as Mobile_median
                                      from adconv_sub_mobile_desktop_holidays_agg group by age_bracket")
str(adconv_mobile_desktop_holidays)

adconv_mobile_desktop_holidays$ratio<-adconv_mobile_desktop_holidays$Mobile_median/adconv_mobile_desktop_holidays$Desktop_median

total_buyers_holidays<-sqldf("select count(distinct userid) from adconv_sub 
                             where chardt>='2014-11-01'and chardt<='2014-12-31'")

#During Holidays- People who bought same product across both devices

advonv_mobile_desktop_buyers_holidays_product<-sqldf("select a.* from adconv_mobile_buyers_holidays a join adconv_desktop_buyers_holidays b on a.userid=b.userid
                                                     and a.ma_category=b.ma_category")
advonv_mobile_desktop_buyers_holidays_product<-sqldf("select a.* from adconv_sub a join advonv_mobile_desktop_buyers_holidays_product b on a.userid=b.userid and 
                                                     a.chardt>='2014-11-01'and a.chardt<='2014-12-31'")
adconv_sub_mobile_desktop_holidays_product_agg<-aggregate(list(usd_amount=advonv_mobile_desktop_buyers_holidays_product$usd_amount), 
                                                          by=list(product_category=advonv_mobile_desktop_buyers_holidays_product$ma_category,
                                                                  conversion_device_new=advonv_mobile_desktop_buyers_holidays_product$conversion_device_new),
                                                          median)

adconv_mobile_desktop_holidays_product<-sqldf("select product_category,
                                              sum(case when conversion_device_new ='Desktop' then usd_amount else 0 end)/100 as Desktop_median,
                                              sum(case when conversion_device_new ='Mobile' then usd_amount else 0 end)/100 as Mobile_median
                                              from adconv_sub_mobile_desktop_holidays_product_agg group by product_category")
adconv_mobile_desktop_holidays_product$ratio<-adconv_mobile_desktop_holidays_product$Mobile_median/adconv_mobile_desktop_holidays_product$Desktop_median

#In the last 6 months- 

# All purchases
adconv_6m<-sqldf("select * from adconv_sub where chardt>'2015-03-01' ")

adconv_sub_6m_agg<-aggregate(list(usd_amount=adconv_6m$usd_amount), 
                             by=list(age_bracket=adconv_6m$age_bracket,conversion_device_new=adconv_6m$conversion_device_new),
                             median)

str(adconv_sub_6m_agg)

adconv_6m_buyers<-sqldf("select age_bracket,
                        sum(case when conversion_device_new ='Desktop' then usd_amount else 0 end)/100 as Desktop_median,
                        sum(case when conversion_device_new ='Mobile' then usd_amount else 0 end)/100 as Mobile_median
                        from adconv_sub_6m_agg group by age_bracket")
str(adconv_6m_buyers)
adconv_6m_buyers$ratio<-adconv_6m_buyers$Mobile_median/adconv_6m_buyers$Desktop_median


#For people who bought both on Mobile and Web


adconv_mobile_buyers_3m<-sqldf("select * from adconv_sub where conversion_device_new in('Mobile','Tablet') and chardt>'2015-03-01' ")

adconv_desktop_buyers_3m<-sqldf("select * from adconv_sub where conversion_device_new='Desktop' and chardt>'2015-03-01'")

advonv_mobile_desktop_buyers_3m<-sqldf("select a.* from adconv_mobile_buyers_3m a join adconv_desktop_buyers_3m b on a.userid=b.userid")
advonv_mobile_desktop_buyers_3m<-sqldf("select a.* from adconv_sub a join advonv_mobile_desktop_buyers_3m b on a.userid=b.userid and 
                                       a.chardt>'2015-03-01'")

str(advonv_mobile_desktop_buyers_3m)
adconv_sub_mobile_desktop_agg<-aggregate(list(usd_amount=advonv_mobile_desktop_buyers_3m$usd_amount), 
                                         by=list(age_bracket=advonv_mobile_desktop_buyers_3m$age_bracket,conversion_device_new=advonv_mobile_desktop_buyers_3m$conversion_device_new),
                                         median)

str(adconv_sub_mobile_desktop_agg)

adconv_mobile_desktop<-sqldf("select age_bracket,
                             sum(case when conversion_device_new ='Desktop' then usd_amount else 0 end)/100 as Desktop_median,
                             sum(case when conversion_device_new ='Mobile' then usd_amount else 0 end)/100 as Mobile_median
                             from adconv_sub_mobile_desktop_agg group by age_bracket")
str(adconv_mobile_desktop)
adconv_mobile_desktop$ratio<-adconv_mobile_desktop$Mobile_median/adconv_mobile_desktop$Desktop_median

#In the last 6 months- For people who bought both on Mobile and Web

advonv_mobile_desktop_buyers_3m_product<-sqldf("select a.* from adconv_mobile_buyers_3m a join adconv_desktop_buyers_3m b on a.userid=b.userid
                                               and a.ma_category=b.ma_category")
advonv_mobile_desktop_buyers_3m_product<-sqldf("select a.* from adconv_sub a join advonv_mobile_desktop_buyers_3m_product b on a.userid=b.userid and 
                                               a.chardt>'2015-03-01' ")
adconv_sub_mobile_desktop_product_agg<-aggregate(list(usd_amount=advonv_mobile_desktop_buyers_3m_product$usd_amount), 
                                                 by=list(product_category=advonv_mobile_desktop_buyers_3m_product$ma_category,
                                                         conversion_device_new=advonv_mobile_desktop_buyers_3m_product$conversion_device_new),
                                                 median)

adconv_mobile_desktop_product<-sqldf("select product_category,
                                     sum(case when conversion_device_new ='Desktop' then usd_amount else 0 end)/100 as Desktop_median,
                                     sum(case when conversion_device_new ='Mobile' then usd_amount else 0 end)/100 as Mobile_median
                                     from adconv_sub_mobile_desktop_product_agg group by product_category")
adconv_mobile_desktop_product$ratio<-adconv_mobile_desktop_product$Mobile_median/adconv_mobile_desktop_product$Desktop_median

#In the last 6 months- For top 5 advertisers in retail and eccomerce

adconv_sub$ultimate_parent_name_new <- gsub("'", '-', adconv_sub$ultimate_parent_name)
adconv_sub_retail_eccom6m<-sqldf("select * from adconv_sub where ultimate_parent_name in ('Liberty Media Corp',
                                 'Overstock.com, Inc','ContextLogic (Wish)','Groupon','LivingSocial Inc','Target Corp','Nordstrom Inc',
                                 'Macy-s Inc','Lowe-s Companies Inc','Newton Holding Llc') and chardt>'2015-03-01' ")


adconv_sub_retail_eccom6m_agg<-aggregate(list(usd_amount=adconv_sub_retail_eccom6m$usd_amount), 
                                         by=list(ds=adconv_sub_retail_eccom6m$ds,conversion_device_new=adconv_sub_retail_eccom6m$conversion_device_new),
                                         median)

str(adconv_sub_retail_eccom6m_agg)

adconv_sub_retail_eccom6m_buyers<-sqldf("select ds,
                                        sum(case when conversion_device_new ='Desktop' then usd_amount else 0 end)/100 as Desktop_median,
                                        sum(case when conversion_device_new ='Mobile' then usd_amount else 0 end)/100 as Mobile_median,
                                        sum(case when conversion_device_new ='Tablet' then usd_amount else 0 end)/100 as Tablet_median
                                        from adconv_sub_retail_eccom6m_agg group by ds")
str(adconv_sub_retail_eccom6m_buyers)
adconv_sub_retail_eccom6m_buyers$ratio_mobile<-adconv_sub_retail_eccom6m_buyers$Mobile_median/adconv_sub_retail_eccom6m_buyers$Desktop_median
adconv_sub_retail_eccom6m_buyers$tablet_ratio<-adconv_sub_retail_eccom6m_buyers$Tablet_median/adconv_sub_retail_eccom6m_buyers$Desktop_median

adconv_sub_retail_eccom6m_buyers_plot<-adconv_sub_retail_eccom6m_buyers[,c(1,5,6)]
adconv_sub_retail_eccom6m_buyers_plot <- melt(adconv_sub_retail_eccom6m_buyers_plot, id="ds")


#In the last 6 months- for top eccommerce + retail by product category

adconv_sub_retail_eccom6m_product<-aggregate(list(usd_amount=adconv_sub_retail_eccom6m$usd_amount), 
                                             by=list(product_category=adconv_sub_retail_eccom6m$ma_category,
                                                     conversion_device_new=adconv_sub_retail_eccom6m$conversion_device_new),
                                             median)

adconv_sub_retail_eccom6m_product_agg<-sqldf("select product_category,
                                             sum(case when conversion_device_new ='Desktop' then usd_amount else 0 end)/100 as Desktop_median,
                                             sum(case when conversion_device_new ='Mobile' then usd_amount else 0 end)/100 as Mobile_median,
                                             sum(case when conversion_device_new ='Tablet' then usd_amount else 0 end)/100 as Tablet_median
                                             from adconv_sub_retail_eccom6m_product group by product_category")
adconv_sub_retail_eccom6m_product_agg$ratio<-adconv_sub_retail_eccom6m_product_agg$Mobile_median/adconv_sub_retail_eccom6m_product_agg$Desktop_median
adconv_sub_retail_eccom6m_product_agg$tablet_ratio<-adconv_sub_retail_eccom6m_product_agg$Tablet_median/adconv_sub_retail_eccom6m_product_agg$Desktop_median

adconv_sub_retail_eccom6m_product_agg_plot<-adconv_sub_retail_eccom6m_product_agg[,c(1,5,6)]
adconv_sub_retail_eccom6m_product_agg_plot <- melt(adconv_sub_retail_eccom6m_product_agg_plot, id="product_category")

#In the last 6 months- for top eccommerce + retail for people who bought on both mobile phone and desktop

adconv_retail_eccom6m_mobile_buyers<-sqldf("select * from adconv_sub_retail_eccom6m where conversion_device_new in ('Mobile') and chardt>='2015-03-01'")

adconv_retail_eccom6m_desktop_buyers<-sqldf("select * from adconv_sub_retail_eccom6m where conversion_device_new='Desktop'and chardt>='2015-03-01'")

adconv_retail_eccom6m_mobile_desktop_buyers<-sqldf("select a.* from adconv_retail_eccom6m_mobile_buyers a join adconv_retail_eccom6m_desktop_buyers b on a.userid=b.userid")
adconv_retail_eccom6m_mobile_desktop_buyers<-sqldf("select a.* from adconv_sub_retail_eccom6m a join adconv_retail_eccom6m_mobile_desktop_buyers b on a.userid=b.userid and 
                                       a.chardt>'2015-03-01'")
adconv_retail_eccom6m_mobile_desktop_agg<-aggregate(list(usd_amount=adconv_retail_eccom6m_mobile_desktop_buyers$usd_amount), 
                                                 by=list(conversion_device_new=adconv_retail_eccom6m_mobile_desktop_buyers$conversion_device_new),
                                                 median)

#In the last 6 months- for top eccommerce + retail for people who bought on both mobile phone and tablet

adconv_retail_eccom6m_tablet_buyers<-sqldf("select * from adconv_sub_retail_eccom6m where conversion_device_new='Tablet'and chardt>='2015-03-01'")

adconv_retail_eccom6m_mobile_tablet_buyers<-sqldf("select a.* from adconv_retail_eccom6m_mobile_buyers a join adconv_retail_eccom6m_tablet_buyers b on a.userid=b.userid")
adconv_retail_eccom6m_mobile_tablet_buyers<-sqldf("select a.* from adconv_sub_retail_eccom6m a join adconv_retail_eccom6m_mobile_tablet_buyers b on a.userid=b.userid and 
                                       a.chardt>'2015-03-01'")
adconv_retail_eccom6m_mobile_tablet_agg<-aggregate(list(usd_amount=adconv_retail_eccom6m_mobile_tablet_buyers$usd_amount), 
                                                    by=list(conversion_device_new=adconv_retail_eccom6m_mobile_tablet_buyers$conversion_device_new),
                                                    median)

#In the last 6 months- for top eccommerce + retail for people who bought on both desktop and tablet


adconv_retail_eccom6m_tablet_desktop_buyers<-sqldf("select a.* from adconv_retail_eccom6m_tablet_buyers a join adconv_retail_eccom6m_desktop_buyers b on a.userid=b.userid")
adconv_retail_eccom6m_tablet_desktop_buyers<-sqldf("select a.* from adconv_sub_retail_eccom6m a join adconv_retail_eccom6m_tablet_desktop_buyers b on a.userid=b.userid and 
                                       a.chardt>'2015-03-01'")
adconv_retail_eccom6m_tablet_desktop_agg<-aggregate(list(usd_amount=adconv_retail_eccom6m_tablet_desktop_buyers$usd_amount), 
                                                    by=list(conversion_device_new=adconv_retail_eccom6m_tablet_desktop_buyers$conversion_device_new),
                                                    median)
#In the last 6 months- for top eccommerce + retail for people who bought on desktop +phone +tablet


adconv_retail_eccom6m_tablet_phone_desktop_buyers<-sqldf("select a.* from adconv_retail_eccom6m_tablet_desktop_buyers a join adconv_retail_eccom6m_mobile_tablet_buyers b on a.userid=b.userid")
adconv_retail_eccom6m_tablet_phone_desktop_buyers<-sqldf("select a.* from adconv_sub_retail_eccom6m a join adconv_retail_eccom6m_tablet_phone_desktop_buyers b on a.userid=b.userid and 
                                                   a.chardt>'2015-03-01'")
adconv_retail_eccom6m_tablet_phone_desktop_agg<-aggregate(list(usd_amount=adconv_retail_eccom6m_tablet_phone_desktop_buyers$usd_amount), 
                                                    by=list(conversion_device_new=adconv_retail_eccom6m_tablet_phone_desktop_buyers$conversion_device_new),
                                                    median)
#In the last 6 months- 

#All purchases
advonv_buyers_6m_a<-sqldf("select * from adconv_sub where chardt>='2015-03-01'")
adconv_sub_6m_a_agg<-aggregate(list(usd_amount=advonv_buyers_6m_a$usd_amount), 
                                   by=list(age_bracket=advonv_buyers_6m_a$age_bracket,conversion_device_new=advonv_buyers_6m_a$conversion_device_new),
                                   median)

str(adconv_sub_6m_a_agg)

adconv_6m_a<-sqldf("select age_bracket,
                       sum(case when conversion_device_new ='Desktop' then usd_amount else 0 end)/100 as Desktop_median,
                       sum(case when conversion_device_new ='Mobile' then usd_amount else 0 end)/100 as Mobile_median,
                       sum(case when conversion_device_new ='Tablet' then usd_amount else 0 end)/100 as Tablet_median
                       from advonv_buyers_6m_a group by age_bracket")
str(adconv_6m_a)

adconv_6m_a$ratio<-adconv_6m_a$Mobile_median/adconv_6m_a$Desktop_median
adconv_6m_a$ratio_tablet<-adconv_6m_a$Tablet_median/adconv_6m_a$Desktop_median
adconv_adconv_6m_a_plot<-adconv_6m_a[,c(1,5,6)]
adconv_adconv_6m_a_plot <- melt(adconv_adconv_6m_a_plot, id="age_bracket")


# For retail and eccomerce
adconv_sub_retail_eccom_all6m<-sqldf("select * from adconv_sub where vertical_name in ('Retail','Ecommerce') and chardt>'2015-03-01' ")


adconv_sub_retail_eccom_all6m_agg<-aggregate(list(usd_amount=adconv_sub_retail_eccom_all6m$usd_amount), 
                                         by=list(ds=adconv_sub_retail_eccom_all6m$ds,conversion_device_new=adconv_sub_retail_eccom_all6m$conversion_device_new),
                                         median)

str(adconv_sub_retail_eccom_all6m_agg)

adconv_sub_retail_eccom_all6m_buyers<-sqldf("select ds,
                                        sum(case when conversion_device_new ='Desktop' then usd_amount else 0 end)/100 as Desktop_median,
                                        sum(case when conversion_device_new ='Mobile' then usd_amount else 0 end)/100 as Mobile_median,
                                        sum(case when conversion_device_new ='Tablet' then usd_amount else 0 end)/100 as Tablet_median
                                        from adconv_sub_retail_eccom_all6m_agg group by ds")
str(adconv_sub_retail_eccom_all6m_buyers)
adconv_sub_retail_eccom_all6m_buyers$ratio_mobile<-adconv_sub_retail_eccom_all6m_buyers$Mobile_median/adconv_sub_retail_eccom_all6m_buyers$Desktop_median
adconv_sub_retail_eccom_all6m_buyers$tablet_ratio<-adconv_sub_retail_eccom_all6m_buyers$Tablet_median/adconv_sub_retail_eccom_all6m_buyers$Desktop_median

adconv_sub_retail_eccom_all6m_buyers_plot<-adconv_sub_retail_eccom_all6m_buyers[,c(1,5,6)]
adconv_sub_retail_eccom_all6m_buyers_plot <- melt(adconv_sub_retail_eccom_all6m_buyers_plot, id="ds")

#In the last 6 months- for top eccommerce + retail by product category

adconv_sub_retail_eccom_all6m_product<-aggregate(list(usd_amount=adconv_sub_retail_eccom_all6m$usd_amount), 
                                             by=list(product_category=adconv_sub_retail_eccom_all6m$ma_category,
                                                     conversion_device_new=adconv_sub_retail_eccom_all6m$conversion_device_new),
                                             median)

adconv_sub_retail_eccom_all6m_product_agg<-sqldf("select product_category,
                                             sum(case when conversion_device_new ='Desktop' then usd_amount else 0 end)/100 as Desktop_median,
                                             sum(case when conversion_device_new ='Mobile' then usd_amount else 0 end)/100 as Mobile_median,
                                             sum(case when conversion_device_new ='Tablet' then usd_amount else 0 end)/100 as Tablet_median
                                             from adconv_sub_retail_eccom_all6m_product group by product_category")
adconv_sub_retail_eccom_all6m_product_agg$ratio<-adconv_sub_retail_eccom_all6m_product_agg$Mobile_median/adconv_sub_retail_eccom_all6m_product_agg$Desktop_median
adconv_sub_retail_eccom_all6m_product_agg$tablet_ratio<-adconv_sub_retail_eccom_all6m_product_agg$Tablet_median/adconv_sub_retail_eccom_all6m_product_agg$Desktop_median

adconv_sub_retail_eccom_all6m_product_agg_plot<-adconv_sub_retail_eccom_all6m_product_agg[,c(1,5,6)]
adconv_sub_retail_eccom_all6m_product_agg_plot <- melt(adconv_sub_retail_eccom_all6m_product_agg_plot, id="product_category")

#In the last 6 months- for top eccommerce + retail + travel by sub vertical


adconv_sub_retail_eccom_travel_all6m<-sqldf("select * from adconv_sub where vertical_name in ('Retail','Ecommerce','Travel') and lower(sub_vertical_name) in
                                            ('department store','restaurant','apparel & accessories','ecatalog','daily deals',
                                            'travel agency','bus/taxi/auto rental','hotel & accomodation') and chardt>'2015-03-01' ")

adconv_sub_retail_eccom_travelall6m_sub_vertical<-aggregate(list(usd_amount=adconv_sub_retail_eccom_travel_all6m$usd_amount), 
                                                 by=list(sub_vertical=adconv_sub_retail_eccom_travel_all6m$sub_vertical_name,
                                                         conversion_device_new=adconv_sub_retail_eccom_travel_all6m$conversion_device_new),
                                                 median)

adconv_sub_retail_eccom_travelall6m_sub_vertical_agg<-sqldf("select sub_vertical,
                                                 sum(case when conversion_device_new ='Desktop' then usd_amount else 0 end)/100 as Desktop_median,
                                                 sum(case when conversion_device_new ='Mobile' then usd_amount else 0 end)/100 as Mobile_median,
                                                 sum(case when conversion_device_new ='Tablet' then usd_amount else 0 end)/100 as Tablet_median
                                                 from adconv_sub_retail_eccom_travelall6m_sub_vertical group by sub_vertical")

adconv_sub_retail_eccom_travelall6m_sub_vertical_agg$ratio<-adconv_sub_retail_eccom_travelall6m_sub_vertical_agg$Mobile_median/adconv_sub_retail_eccom_travelall6m_sub_vertical_agg$Desktop_median
adconv_sub_retail_eccom_travelall6m_sub_vertical_agg$tablet_ratio<-adconv_sub_retail_eccom_travelall6m_sub_vertical_agg$Tablet_median/adconv_sub_retail_eccom_travelall6m_sub_vertical_agg$Desktop_median

adconv_sub_retail_eccom_travelall6m_product_agg_plot<-adconv_sub_retail_eccom_travelall6m_sub_vertical_agg[,c(1,5,6)]
adconv_sub_retail_eccom_travelall6m_product_agg_plot <- melt(adconv_sub_retail_eccom_travelall6m_product_agg_plot, id="sub_vertical")

# density - threshold plots

mobile_data<-subset(adconv_sub,conversion_device_new=='Mobile')
tablet_data<-subset(adconv_sub,conversion_device_new=='Tablet')
desktop_data<- subset(adconv_sub,conversion_device_new=='Desktop')
desktop_quantile<-quantile(desktop_data$usd_amount,c(.05,.95))
mobile_quantile<-quantile(mobile_data$usd_amount,c(.05,.95))
tablet_quantile<-quantile(tablet_data$usd_amount,c(.05,.95))

ax<-ggplot(adconv_sub_new, aes(x = conversion_device_new, y = round((usd_amount/100),2),label=round(usd_amount,2),fill=conversion_device_new)) +
  geom_boxplot()+ xlab("Purchasing device") + ylab("Amount spent on desktop, mobile and tablet") +
  ggtitle("Thresholds of amount spent on Desktop/Mobile/Tablet")

#plots



x<-ggplot(adconv_sub_agg_plot, aes(x=purchase_type,y=ratio,label=round(ratio,2))) + 
  geom_bar(position=position_dodge(),stat="identity") +
  geom_text(vjust=-.3, size=2,colour="blue")+
  theme(axis.text.x = element_text(hjust = .3, size = 4, colour = "grey40"),
        axis.title=element_text(size=8))+
  xlab("Purchase type") + ylab("Ratio of Amount spent on Mobile/Tablet vs Desktop") +
  ggtitle("Ratio of amount spent on Mobile/Tablet vs Desktop")


xa<-ggplot(adconv_sub_agg_plot_msite, aes(x=purchase_type,y=ratio,label=round(ratio,2))) + 
  geom_bar(position=position_dodge(),stat="identity") +
  geom_text(vjust=-.3, size=2,colour="blue")+
  theme(axis.text.x = element_text(hjust = .3, size = 4, colour = "grey40"),
        axis.title=element_text(size=8))+
  xlab("Purchase type") + ylab("Ratio of Amount spent on Msite/Mobile app vs Desktop") +
  ggtitle("Ratio of amount spent on Msite/Mobile app vs Desktop")


y<-ggplot(adconv_device_any, aes(x=ds,y=ratio_phone)) + 
  geom_line(size=1.2) + 
  geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 4, colour = "grey40"),
        axis.title=element_text(size=8))+
 xlab("date") + ylab("Ratio of Amount spent on Mobile phone vs Desktop")

ya<-ggplot(adconv_device_any, aes(x=ds,y=ratio_tablet)) + 
  geom_line(size=1.2,colour='blue') + 
  geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 4, colour = "grey40"),
        axis.title=element_text(size=8)) +
  xlab("date") + ylab("Ratio of Amount spent on Tablet vs Desktop")
  
z<-ggplot(adconv_device_mobile_desktop_overall_date, aes(x=ds,y=ratio_mobile)) + 
  geom_line(size=1.2) + 
  geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 4, colour = "grey40"),
        axis.title=element_text(size=8)) +
  xlab("date") + ylab("Ratio of Amount spent on Mobile vs Desktop by
                      people who bought on both devices") 

za<-ggplot(adconv_device_mobile_desktop_overall_date, aes(x=ds,y=tablet_ratio)) + 
  geom_line(size=1.2,colour='blue') + 
  geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 4, colour = "grey40"),
        axis.title=element_text(size=8)) +
  xlab("date") + ylab("Ratio of Amount spent on Mobile vs Desktop by
                      people who bought on both devices")   


k<-ggplot(adconv_sub_retail_eccom6m_buyers, aes(x=ds,y=ratio_mobile)) + 
  geom_line(size=1.2) + 
  geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 4, colour = "grey40"),
        axis.title=element_text(size=8)) +
  xlab("date") + ylab("Ratio of Amount spent on Mobile vs Desktop for top retail+ecommerce")


kx<-ggplot(adconv_sub_retail_eccom6m_buyers, aes(x=ds,y=tablet_ratio)) + 
  geom_line(size=1.2,colour='blue') + 
  geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 4, colour = "grey40"),
        axis.title=element_text(size=8)) +
  xlab("date") + ylab("Ratio of Amount spent on Tablet vs Desktop for top retail+ecommerce")


m<-ggplot(adconv_sub_retail_eccom6m_product_agg_plot, aes(x=product_category, y=value, colour=variable, label=round(value,2))) +
  geom_bar(position=position_dodge(),stat="identity") +geom_text(vjust=-.1, size=2,colour="blue")+ theme_bw(16)+
  xlab("Product type") + ylab("Ratio of Amount spent on Mobile vs Desktop") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8, colour = "grey40")) +
  ggtitle(expression(atop("Ratio of amount spent on Mobile vs Desktop", 
                          atop(italic("for products bought from top ecommerce+retail since March 2015")," "))))



ka<-ggplot(adconv_sub_retail_eccom_all6m_buyers, aes(x=ds,y=ratio_mobile)) + 
  geom_line(size=1.2) + 
  geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 4, colour = "grey40"),
        axis.title=element_text(size=8)) +
  xlab("date") + ylab("Ratio of Amount spent on Mobile vs Desktop for all retail+ecommerce")


kax<-ggplot(adconv_sub_retail_eccom_all6m_buyers, aes(x=ds,y=tablet_ratio)) + 
  geom_line(size=1.2,colour='blue') + 
  geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 4, colour = "grey40"),
        axis.title=element_text(size=8)) +
  xlab("date") + ylab("Ratio of Amount spent on Tablet vs Desktop for all retail+ecommerce")


ma<-ggplot(adconv_sub_retail_eccom_all6m_product_agg_plot, aes(x=product_category, y=value, colour=variable, label=round(value,2))) +
  geom_bar(position=position_dodge(),stat="identity") +geom_text(vjust=-.1, size=2,colour="blue")+ theme_bw(16)+
  xlab("Product type") + ylab("Ratio of Amount spent on Mobile vs Desktop") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8, colour = "grey40")) +
  ggtitle(expression(atop("Ratio of amount spent on Mobile vs Desktop", 
                          atop(italic("for products bought from all ecommerce+retail since March 2015")," "))))


mab<-ggplot(adconv_sub_retail_eccom_travelall6m_product_agg_plot, aes(x=sub_vertical, y=value, colour=variable, label=round(value,2))) +
  geom_bar(position=position_dodge(),stat="identity") +geom_text(vjust=-.1, size=2,colour="blue")+ theme_bw(16)+
  xlab("Product type") + ylab("Ratio of Amount spent on Mobile vs Desktop") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8, colour = "grey40")) +
  ggtitle(expression(atop("Ratio of amount spent on Mobile vs Desktop", 
                          atop(italic("for sub-verticals bought from all ecommerce+retail+travel since March 2015")," "))))


c<-ggplot(adconv_mobile_desktop_product, aes(x=product_category, y=ratio, label=round(ratio,2)))+
  geom_bar(position=position_dodge(),stat="identity") + geom_text(vjust=-.3, size=2,colour="blue")+theme_bw(16)+
  xlab("Product type") + ylab("Ratio of Amount spent on Mobile vs Desktop") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8, colour = "grey40")) +
  ggtitle(expression(atop("Ratio of amount spent on Mobile vs Desktop", 
                          atop(italic("by people who bought same products on both devices since March 2015")," "))))

a<-ggplot(adconv_6m_buyers, aes(x=age_bracket, y=ratio,label=round(ratio,2))) +
  geom_bar(position=position_dodge(),stat="identity") + geom_text(vjust=-.3,size=2, colour="blue")+theme_bw(16)+
  xlab("Age Bracket") + ylab("Ratio of Amount spent on Mobile vs Desktop") +
  ggtitle(expression(atop("Ratio of amount spent on Mobile vs Desktop", 
                          atop(italic("by all people since March 2015")," "))))

b<-ggplot(adconv_mobile_desktop, aes(x=age_bracket, y=ratio,label=round(ratio,2))) +
  geom_bar(position=position_dodge(),stat="identity") + geom_text(vjust=-.3,size=2, colour="blue")+theme_bw(16)+
  xlab("Age Bracket") + ylab("Ratio of Amount spent on Mobile vs Desktop") +
  ggtitle(expression(atop("Ratio of amount spent on Mobile vs Desktop", 
                          atop(italic("by people who bought on both devices since March 2015")," "))))

ta<-ggplot(adconv_adconv_6m_a_plot, aes(x=age_bracket,y=value, colour=variable,label=round(value,2))) +
  geom_bar(position=position_dodge(),stat="identity") + geom_text(vjust=0, size=5,colour="blue")+ theme_bw(16)+
  xlab("Age Bracket") + ylab("Ratio of Amount spent on Mobile vs Desktop") +
  ggtitle(expression(atop("Ratio of amount spent on Mobile vs Desktop", 
                          atop(italic("includes purchases from all people since March 2015")," "))))

t<-ggplot(adconv_holidays_plot, aes(x=age_bracket,y=value, colour=variable,label=round(value,2))) +
  geom_bar(position=position_dodge(),stat="identity") + geom_text(vjust=0, size=5,colour="blue")+ theme_bw(16)+
  xlab("Age Bracket") + ylab("Ratio of Amount spent on Mobile vs Desktop") +
  ggtitle(expression(atop("Ratio of amount spent on Mobile vs Desktop", 
                          atop(italic("includes purchases from all people during holidays Nov-Dec 2014")," "))))


u<-ggplot(adconv_mobile_desktop_holidays, aes(x=age_bracket, y=ratio,label=round(ratio,2))) +
  geom_bar(position=position_dodge(),stat="identity") + geom_text(vjust=-.3,size=2, colour="blue")+ theme_bw(16)+
  xlab("Age Bracket") + ylab("Ratio of Amount spent on Mobile vs Desktop") +
  ggtitle(expression(atop("Ratio of amount spent on Mobile vs Desktop", 
                          atop(italic("by people who bought on both devices during holidays Nov-Dec 2014")," "))))

v<-ggplot(adconv_mobile_desktop_holidays_product, aes(x=product_category, y=ratio,label=round(ratio,2))) +
  geom_bar(position=position_dodge(),stat="identity") +geom_text(vjust=-.3,size=2, colour="blue")+ xlab("Product type") + 
  ylab("Ratio of Amount spent on Mobile vs Web") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8, colour = "grey40")) +
  ggtitle(expression(atop("Ratio of amount spent on Mobile vs Desktop", 
                          atop(italic("by people who bought same products on both devices during holidays Nov-Dec 2014")," "))))




path<-paste("Pricing analysis_mob_tab_desk_6", ".pdf", sep = "")
pdf(file=path)

print(ax)
print(desktop_quantile/100)
print(mobile_quantile/100)
print(tablet_quantile/100)

print(x)

print(xa)

print(adconv_sub_mobile_desktop_overall_agg)

print(grid.arrange(y, ya,nrow=2))
                                                            
print(grid.arrange(z, za,nrow=2))

print(adconv_sub_msite_app_freq_people)

print(grid.arrange(k, kx,nrow=2))


print(m)

print(grid.arrange(ka, kax,nrow=2))

print(ma)

print(mab)

print(c)

print(a)

print(ta)

print(b)

print(t)

print(u)

print(v)

print(adconv_retail_eccom6m_mobile_desktop_agg)

print(adconv_retail_eccom6m_tablet_desktop_agg)

print(adconv_retail_eccom6m_tablet_phone_desktop_agg)

dev.off()  







