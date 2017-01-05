x<-ggplot(adconv_sub_agg_plot, aes(x=purchase_type,y=ratio,label=round(ratio,2))) + 
  geom_bar(position=position_dodge(),stat="identity") +
  geom_text(vjust=-.3, size=2,colour="blue")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8, colour = "grey40"))+
  xlab("Purchase type") + ylab("Ratio of Amount spent on Mobile vs Desktop") +
  ggtitle("Ratio of amount spent on Mobile vs Desktop")


y<-ggplot(adconv_device_any, aes(x=ds,y=ratio)) + 
  geom_line(size=1.2) + 
  geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1))+
  theme_bw(16) +
  xlab("date") + ylab("Ratio of Amount spent on Mobile vs Desktop") +
  ggtitle(expression(atop("Ratio of amount spent on Mobile vs Desktop overall", 
                          atop(italic("by date since March 2015")," "))))

k<-ggplot(adconv_sub_retail_eccom6m_buyers, aes(x=ds,y=ratio)) + 
  geom_line(size=1.2) + 
  geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1))+
  theme_bw(16) +
  xlab("date") + ylab("Ratio of Amount spent on Mobile vs Desktop") +
  ggtitle(expression(atop("Ratio of amount spent on Mobile vs Desktop overall", 
                          atop(italic("for top retail + eccommerce by date since March 2015")," "))))

m<-ggplot(adconv_sub_retail_eccom6m_product_agg, aes(x=product_category, y=ratio, label=round(ratio,2))) +
  geom_bar(position=position_dodge(),stat="identity") +geom_text(vjust=-.3, size=2,colour="blue")+ theme_bw(16)+
  xlab("Product type") + ylab("Ratio of Amount spent on Mobile vs Desktop") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8, colour = "grey40")) +
  ggtitle(expression(atop("Ratio of amount spent on Mobile vs Desktop", 
                          atop(italic("for products bought from top ecommerce+retail since March 2015")," "))))



z<-ggplot(adconv_device_mobile_desktop_overall_date, aes(x=ds,y=ratio)) + 
  geom_line(size=1.2) + 
  geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1))+
  theme_bw(16) +
  xlab("date") + ylab("Ratio of Amount spent on Mobile vs Desktop") +
  ggtitle(expression(atop("Ratio of amount spent on Mobile vs Desktop overall", 
                          atop(italic("includes purchases from people who bought on both devices since March 2015")," "))))


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



t<-ggplot(adconv_holidays, aes(x=age_bracket, y=ratio,label=round(ratio,2))) +
  geom_bar(position=position_dodge(),stat="identity") + geom_text(vjust=-.3, size=2,colour="blue")+ theme_bw(16)+
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




path<-paste("Pricing analysis_mobile_vs_desktop", ".pdf", sep = "")
pdf(file=path)

print(x)

print(y)

print(k)

print(m)
print(z)

print(c)

print(a)

print(b)



print(t)

print(u)

print(v)



dev.off()  