####### LEACH-MASON Royalty simulations Nov 2021 #######
#Mac
if(R.version$platform ==  "x86_64-apple-darwin15.6.0")
  setwd("/Users/aleach/Google Drive/Papers/Royalties")
#PC
if(R.version$platform ==  "x86_64-w64-mingw32")
  setwd("C:/Users/aleach/Google Drive/Papers/Royalties")
print(getwd())

print(getwd())
source("../../andrew_base.R")


#specific to this project

library(nleqslv)
library(numDeriv)
library(cowplot)


#reserve discovery function
ee_cost_exp <- 3
ee_cost_base <- 50
ee_cost_mult <- .000005
ee_cost_scale<-1
ee_cost_shift<-1


#### BASE MODEL ###
reserves<-(c(0:200)/200)^(2)*2000+.01



#extensive margin - extraction cost 

ext_cost_exp <- 4
ext_cost_mult <- 50
ext_cost_base <- 12
ext_cost_res_factor <- 1/180
extract<-c(0:30)



#POLICY PARAMETERS

res_carb_tax<-0
ext_carb_tax<-30
cons_carb_tax<-0 #impact of consumption carbon taxes on oil prices
net_rev_rate<-0.1
gross_rev_rate<-0

oba_rate<-0.8 #percentage of average eibbls returned via OBA
oba_val<-30 #value of OBA credits, in $/tonne

#oba_rate<-0 #percentage of average eibbls returned via OBA
#oba_val<-0 #value of OBA credits, in $/tonne


#fixed GHG parameters
eibbls<-0.55
incidence<-0.5

#abatement costs
tac_1<-250
tac_exp_b<- 2
tac_exp_cc<- 6
tac_exp_cv<-1.15  




#here, we need to make a scenarios table
oil_price<<-45
abate_curve<-"abate_base"

price_vec<-c(80)
carbon_taxes<-c(50)
tax_type<-c("extraction","reserves")
royalties<-c(0,0.3)
roy_type<-c("net","gross")
oba_rates<-c(0,0.8)

abate_curves<-c("abate_base","abate_convex")

#define scenarios
#testing scenarios
#11) no carbon tax, gross revenue royalties
#scenarios<-c(1,price_vec[1],0,"extraction",0.2,"gross",0,"abate_base","gross-revenue royalty (.2)")
#12) no carbon tax, gross revenue royalties
#scenarios<-scenarios%>% rbind(c(2,price_vec[1],0,"extraction",0.201,"gross",0,"abate_base","gross-revenue royalty (.23)"))
#13) no carbon tax, gross revenue royalties
#scenarios<-scenarios%>% rbind(c(3,price_vec[1],0,"extraction",0.202,"gross",0,"abate_base","gross-revenue royalty (.26)"))
#14)
#scenarios<-scenarios%>% rbind(c(4,price_vec[1],0,"extraction",0.3,"net",0,"abate_base","net-revenue royalty"))





#1) base case
scenarios<-c(1,price_vec[1],0,"extraction",0,"gross",0,"abate_base","base case")
#2) carbon tax on extraction
scenarios<-scenarios%>% rbind(c(2,price_vec[1],50,"extraction",0,"gross",0,"abate_base","carbon tax on extraction"))
#use this one if you're making the MAC graphs'
#scenarios<-scenarios%>% rbind(c(2,price_vec[1],50,"extraction",0,"gross",0,"abate_base","carbon tax on extraction with linear marginal abatement cost"))
#3) carbon tax on reserves
scenarios<-scenarios%>% rbind(c(3,price_vec[1],50,"reserves",0,"gross",0,"abate_base","carbon tax on reserves"))
#4) carbon tax on extraction w 80% OBA
scenarios<-scenarios%>% rbind(c(4,price_vec[1],50,"extraction",0,"gross",0.8,"abate_base","carbon tax on extraction with 80% OBA"))
#5) carbon tax on extraction with gross revenue royalties
scenarios<-scenarios%>% rbind(c(5,price_vec[1],50,"extraction",0.202,"gross",0,"abate_base","carbon tax on extraction and gross-revenue royalty"))
#6) carbon tax on extraction with net revenue royalties
scenarios<-scenarios%>% rbind(c(6,price_vec[1],50,"extraction",0.3,"net",0,"abate_base","carbon tax on extraction and net-revenue royalty"))
#7) carbon tax on extraction with convext abatement cost
scenarios<-scenarios%>% rbind(c(7,price_vec[1],50,"extraction",0,"gross",0,"abate_convex","carbon tax on extraction and convex marginal abatement cost"))
#8) carbon tax on extraction with concave abatement cost
scenarios<-scenarios%>% rbind(c(8,price_vec[1],50,"extraction",0,"gross",0,"abate_concave","carbon tax on extraction and concave marginal abatement cost"))
#9) no carbon tax, gross revenue royalties
scenarios<-scenarios%>% rbind(c(9,price_vec[1],0,"extraction",0.202,"gross",0,"abate_base","gross-revenue royalty"))
#10) no carbon tax, net revenue royalties
scenarios<-scenarios%>% rbind(c(10,price_vec[1],0,"extraction",0.3,"net",0,"abate_base","net-revenue royalty"))



scenarios<-as_tibble(scenarios,.name_repair = c("minimal"))
names(scenarios)<-c("Number","Oil Price","Carbon Tax","Tax Type","Royalty","Base","OBA","MAC_TYPE","string")
scenarios$`Oil Price` <- as.numeric(as.character(scenarios$`Oil Price`))
scenarios$`Carbon Tax` <- as.numeric(as.character(scenarios$`Carbon Tax`))
scenarios$`Royalty` <- as.numeric(as.character(scenarios$`Royalty`))
scenarios$Number <- as.numeric(as.character(scenarios$Number))
scenarios$OBA <- as.numeric(as.character(scenarios$OBA))

scenarios<-scenarios%>%mutate(mc_string=ifelse(`Tax Type`=="reserves","carbon tax applied to emissions embodied in reserves","base case"),
                              mv_string=ifelse(`Tax Type`=="reserves","base case",string))





scenario_string2<-function(s,fig_num){ #fignum lets me set different labels for different figures
  subsample<-scenarios[s,]  
  return(cat(paste("Scenario ",s,": Oil Price $",subsample$`Oil Price`,"/bbl, Carbon Price $",subsample$`Carbon Tax`,"/t on ",subsample$`Tax Type`,sep=""),
             paste("Royalties applied on ",subsample$Base," revenues",", output-based allocations at ",(subsample$OBA)*100,"% of average cost",sep=""),
             paste("Abatement cost curve: ",subsample$`MAC_TYPE`,sep=""), 
             sep="\n"))
}


scenario_string<-function(s,fig_num){ #fignum lets me set different labels for different figures
  subsample<-scenarios[s,]  
  return(paste("Sc #",s,": Oil $",subsample$`Oil Price`,"/bbl, CTax $",subsample$`Carbon Tax`,"/t on ",subsample$`Tax Type`,", ",subsample$`Royalty`*100,"% ",subsample$Base," revenue royalties",", OBAs at ",(subsample$OBA)*100,"% of avg cost, MAC ",subsample$`MAC_TYPE`,sep=""))
}


scenario_string_clip<-function(s,fig_num,col_sent){ #fignum lets me set different labels for different figures
  subsample<-scenarios[s,]  
  subsample$Royalty<-subsample$Royalty*100
  subsample$OBA <-subsample$OBA*100
  levels(subsample$MAC_TYPE) <- list(Linear="abate_base", Convex="abate_convex", Concave="abate_concave")
  lead_str_vec<-c("Oil price: $","$", "", "Royalty rate: " ,   "Royalty base: "  ,     "OBA rate: "  , "")
  follow_str_vec<-c("/bbl","/t on ", ".", "%" ,   " revenues"  ,     "%"  , " MAC")
  test<-cbind(lead_str_vec,t(subsample[seq(2,8)]),follow_str_vec)
  test<-test[(col_sent[seq(2,8)]>0),]
  seps<-lead_str_vec[(col_sent[seq(2,8)]>0)]
  #seps[]<-". "
  #seps[length(seps)]<-"."
  #print(subsample$MAC_TYPE)
  #paste("Scenario ",s,": ",paste(c(t(cbind(test,seps))), collapse=''),sep="")
  paste(paste(c(t(cbind(test,seps))), collapse=''),sep="")
}


reserve_cost <- function(bbls,res_c_tax=res_carb_tax) {#if not specified, use the global
  #send barrels in billions, get costs in dollars
  cost <- ee_cost_base+ee_cost_mult * bbls^(ee_cost_exp/ee_cost_scale)+bbls*res_c_tax*eibbls
  
  return(cost)
}


marg_reserve_cost <- function(bbls,res_c_tax=res_carb_tax) { #if not specified, use the global
  #send barrels in billions, get costs in dollars
  #cost<- ee_cost_base*ee_cost_mult^(bbls/ee_cost_scale-ee_cost_shift)+res_carb_tax*eibbls
  
  cost <- cost <- ee_cost_exp * ee_cost_mult * bbls^(ee_cost_exp-1)+res_c_tax*eibbls
  return(cost)
}

Fig1_gg<-function(file_sent,width_sent=12,height_sent=6,file_exp,no_ticks=1){
  #Figure 1
  ext_plot<-ggplot(data=tibble(reserves))+
    geom_line(aes(reserves,marg_reserve_cost(reserves,30),color="B",lty="B"),size=rel(2))+
    geom_line(aes(reserves,marg_reserve_cost(reserves,0),color="A",lty="A"),size=rel(2))+
    scale_x_continuous(breaks=pretty_breaks(),expand=c(0,0))+
    scale_y_continuous(breaks=pretty_breaks(),expand=c(0,0))+
    expand_limits(x=c(0,max(reserves)+50),y=c(0,80))+
    scale_color_manual("",values=c("black","grey70"),labels=c("Marginal reserve development cost with no carbon tax on emissions embodied in reserves",paste("Marginal reserve development cost with a ",ifelse(no_ticks==0,"$30/tonne",""),"carbon tax on emissions embodied in reserves",sep = "")))+
    scale_linetype_manual("",values=c("solid","33"),labels=c("Marginal reserve development cost with no carbon tax on emissions embodied in reserves",paste("Marginal reserve development cost with a ",ifelse(no_ticks==0,"$30/tonne",""),"carbon tax on emissions embodied in reserves",sep = "")))+
    labs(x="Developed barrels of proved reserves (millions)",y="Marginal cost per barrel of reserves created ($/bbl)")+
    theme_classic()+
    guides(color=guide_legend(nrow=2,byrow=TRUE,keywidth = 6))+
    theme(
      #legend.position=c(.25,.9),
      text = element_text(size = 14),
      legend.position = "bottom",
    )
  if(no_ticks==1){
    ext_plot<-ext_plot+theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
      axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))
    )
  }
  ext_plot
  if(file_exp==1)
    ggsave(file=file_sent, width = width_sent, height = height_sent,dpi=300)
}

Fig1_gg("reserve_cost.png",file_exp = 1)


#set output based allocation on the basis of barrels
oba<- function(bbls) {
  oba_rate*eibbls  
}



#function for net total carbon costs, including impact of output-based allocations
tot_tax<- function(ghgs,bbls) {
  ext_carb_tax*ghgs-oba(bbls)*oba_val*bbls
}


#function for marginal change in net total carbon costs with change in bbls
dtax_dbbls<- function(ghgs,bbls) {
  -oba(bbls)*oba_val*bbls^0 #use bbls^0 to get the length
}


#function for marginal change in net total carbon costs with change in ghgs
dtax_dghgs<- function(ghgs,bbls) {
  ext_carb_tax*bbls^0 #use bbls^0 to get the length
}



#here type is abate_base unless specified
total_abate_cost <- function(ghgs,bbls,type="abate_base") {
  #set them so they are equal at the minimum of ei=eibbls
  if(type=="abate_base")
    tac_exp<-tac_exp_b
  if(type=="abate_convex")
    tac_exp<-tac_exp_cv
  if(type=="abate_concave")
    tac_exp<-tac_exp_cc
  ei<-ghgs/bbls
  min_ghg<-eibbls*bbls
  c_shift<- (tac_1-tac_1/tac_exp)*eibbls*bbls
  cost <- -tac_1*ghgs+tac_1*eibbls*bbls/tac_exp*(ei/eibbls)^(tac_exp)+c_shift
  return(cost)
}




#calculate marginal change abatement costs for a change in emissions
#here type is abate_base unless specified
dac_dghgs <- function(ghgs,bbls,type="abate_base") {
  if(type=="abate_base")
    tac_exp<-tac_exp_b
  if(type=="abate_convex")
    tac_exp<-tac_exp_cv
  if(type=="abate_concave")
    tac_exp<-tac_exp_cc
  #print(ghgs)
  ei<-ghgs/bbls
  cost <- tac_1-tac_1*(ei/eibbls)^(tac_exp-1)
  return(cost)
}



#calculate marginal change in abatement costs for a change in bbls
#here type is abate_base unless specified
dac_dbbls <- function(ghgs,bbls,type="abate_base") {
  #set them so they are equal at the minimum of ei=eibbls
  if(type=="abate_base")
    tac_exp<-tac_exp_b
  if(type=="abate_convex")
    tac_exp<-tac_exp_cv
  if(type=="abate_concave")
    tac_exp<-tac_exp_cc
  cost <- tac_1 * eibbls/tac_exp * (ghgs/bbls/eibbls)^(tac_exp) - tac_1 * eibbls * 
    bbls/tac_exp * ((ghgs/bbls/eibbls)^((tac_exp) - 1) * ((tac_exp) * (ghgs/bbls^2/eibbls))) + 
    (tac_1 - tac_1/tac_exp) * eibbls
  return(cost)
}


Fig_mac<-function(file_sent,width_sent,height_sent,file_exp,no_ticks=1){
  ghg_data=tibble(ghgs=seq(0,10,.01)) %>% mutate(concave=dac_dghgs(ghgs,15,type="abate_concave"),
                                                 base=dac_dghgs(ghgs,15,type="abate_base"),
                                                 convex=dac_dghgs(ghgs,15,type="abate_convex"))
  mac_plot<-ggplot(ghg_data)+
    geom_line(aes(ghgs,base,col="A",lty="A"),size=rel(2))+
    geom_line(aes(ghgs,concave,col="B",lty="B"),size=rel(2))+
    geom_line(aes(ghgs,convex,col="C",lty="C"),size=rel(2))+
    scale_y_continuous(breaks=pretty_breaks(),expand=c(0,0),limits=c(0,250))+
    scale_x_continuous(breaks=pretty_breaks(),expand=c(0,0),limits=c(0,9))+
    scale_color_manual("",values=c("black","grey70","grey40"),labels=c("Linear MAC","Concave MAC","Convex MAC"))+
    scale_linetype_manual("",values=c("solid","11","33"),labels=c("Linear MAC","Concave MAC","Convex MAC"))+
    labs(x="Emissions",y="Marginal Abatement Costs ($/tonne)")+
    theme_classic()+
    guides(color=guide_legend(nrow=1,byrow=TRUE,keywidth = 6))+
    theme(
      #legend.position=c(.25,.9),
      text = element_text(size = 14),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      
      legend.position = "bottom",
    )
  
  if(no_ticks==1){
    mac_plot<-mac_plot+theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
      axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))
    )
  }
  mac_plot
  
  if(file_exp==1)
    ggsave(file=file_sent, width = width_sent, height = height_sent,dpi=300)
}

#Fig_mac("ma_cost.png",12,6,file_exp=1)

#extraction costs

tot_ext_cost <- function(ghgs,bbls,res,abate_type="abate_base") {
  #send barrels in billions, get costs in dollars
  #bbls<-10
  #ghgs<-10*eibbls
  #res<-1000
  cost <- ext_cost_base*(bbls) + ext_cost_mult * (bbls)^(ext_cost_exp)*ext_cost_res_factor/(res+1)+tot_tax(ghgs,bbls)+total_abate_cost(ghgs,bbls,abate_type)
  #cost<-reserves*opt_ext^2
  return(cost)
}

marg_ext_cost <- function(ghgs,bbls,res,abate_type="abate_base") {
  #send barrels in billions, get costs in dollars
  cost <- ext_cost_base+ ext_cost_exp*ext_cost_mult*(bbls)^(ext_cost_exp-1)*ext_cost_res_factor/(res+1)+dac_dbbls(ghgs,bbls,abate_type)+dtax_dbbls(ghgs,bbls)
  #print(ghgs)
  #cost <- ext_cost_base+ ext_cost_exp*ext_cost_mult*(bbls)^(ext_cost_exp-1)*ext_cost_res_factor/(res+1)
  return(cost)
}


ggFig2<-function(file_sent,width_sent,height_sent,file_exp,no_ticks=1){
  old_oba<-oba_rate
  oba_rate<<-0
  old<-ext_carb_tax
  ext_carb_tax<<-30
  temp_ext<-tibble(ext=seq(0.005,30,.05)) %>% mutate(
    c_tax=tot_ext_cost(ext*eibbls,ext,500,abate_curve)/ext)
  ext_carb_tax<<-0
  temp_ext<-temp_ext %>% mutate(no_c_tax=tot_ext_cost(ext*eibbls,ext,500,abate_curve)/ext)
  
  ext_plot<-ggplot(temp_ext)+
    geom_line(aes(ext,c_tax,color="B",lty="B"),size=rel(2))+
    geom_line(aes(ext,no_c_tax,color="A",lty="A"),size=rel(2))+
    scale_y_continuous(breaks=pretty_breaks(),expand=c(0,0))+
    scale_x_continuous(breaks=pretty_breaks(),expand=c(0,0))+
    expand_limits(x=0,y=0)+
    scale_color_manual("",values=c("black","grey70"),labels=c("Average per-barrel extraction cost","Average per-barrel extraction cost with carbon tax"))+
    scale_linetype_manual("",values=c("solid","33"),labels=c("Average per-barrel extraction cost","Average per-barrel extraction cost with carbon tax"))+
    labs(x="Extraction",y="Average per-barrel cost of extraction ($/bbl)")+
    theme_classic()+
    guides(color=guide_legend(nrow=1,byrow=TRUE,keywidth = 6))+
    theme(
      #legend.position=c(.25,.9),
      text = element_text(size = 14),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      
      legend.position = "bottom",
    )
  if(no_ticks==1){
    ext_plot<-ext_plot+theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
      axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))
    )
  }
  
  if(file_exp==1)
    ggsave(file=file_sent, width = width_sent, height = height_sent,dpi=300)  
  ext_carb_tax<<-old
  oba_rate<<-old_oba
}

ggFig2("extraction_cost_gg.png",14,6,file_exp=1)


net_rev_royalty<- function(p_oil,bbls) {
  return(net_rev_rate)
  #return(0.1)
}

gross_rev_royalty<- function(p_oil,bbls) {
  return(gross_rev_rate)
}


cons_ext_full <- function(x,res_pass){
  rows_x <- length(x)/3 #2 choice variables plus the extraction constraint shadow value
  #print(paste("x has ",rows_x," rows",sep=""))
  x_1 <- x[1:rows_x]
  x_2 <- x[(rows_x+1):(rows_x*2)]
  x_3 <- x[(rows_x*2+1):(rows_x*3)]
  
  #eq1<- marg_ext_cost(x_1,reserves)+.95*m_value(reserves-x_1)-oil_price+x_2
  #marginal extraction cost equals marginal revenue product
  eq1<- (1-net_rev_royalty(oil_price))*marg_ext_cost(x_3,x_1,res_pass,abate_curve)+.95*sim_der(res_pass-x_1)-(1-net_rev_royalty(oil_price))*(1-gross_rev_royalty(oil_price))*(oil_price-cons_carb_tax*(x_3/x_1)*incidence)+x_2
  #eq1<-eq1*5
  #eq1<- marg_ext_cost(x_1,reserves)-oil_price+x_2
  #kuhn tucker condition for extraction greater than reserves
  eq2 <- (res_pass-x_1)*x_2
  #eq2<-eq2*100
  #marginal abatement cost equals marginal cost of carbon emissions
  #as x_3 gets smaller, percentage reduction gets larger
  
  #dec17 debug - change type be be global abate_curve
  #eq_3<-dac_dghgs(x_3,x_1,type="abate_base")-dtax_dghgs(x_3,x_1)
  eq_3<-dac_dghgs(x_3,x_1,type=abate_curve)-dtax_dghgs(x_3,x_1)
  #eq_3<-x_3-x_1*eibbls
  return(c(eq1,eq2,eq_3))
}


deriv_coef<-function(x) {
  x <- coef(x)
  stopifnot(names(x)[1]=="(Intercept)")
  y <- x[-1]
  stopifnot(all(grepl("^poly", names(y))))
  px <- as.numeric(gsub("poly\\(.*\\)","",names(y)))
  rr <- setNames(c(y * px, 0), names(x))
  rr[is.na(rr)] <- 0
  rr
}



fit_value<<-lm((reserves*0)~ poly(reserves, 12, raw=TRUE))
slope <- model.matrix(fit_value) %*% matrix(deriv_coef(fit_value), ncol=1)
der_func <- lm(slope ~ poly(reserves, 12, raw=TRUE))

sim_der<-function(res_data){predict(der_func,data.frame(res_vals=res_data))}
ipolate<-function(res_data){predict(fit_value,data.frame(reserves=res_data))}



model_solve <- function(value_sent=reserves*0)
{
  #initial guess
  value_h <- value_sent
  crit <- 10
  iter <- 1
  #print(lambda)
  lambda <- numeric(NROW(reserves))+12
  res_ext <- pmin((reserves*.25),45)
  ghgs <- res_ext*eibbls
  x_test <- c(res_ext,lambda,ghgs)
  x_2 <- numeric(NROW(reserves))+10
  while(crit>.0005){  
    fit_value<<-lm(value_h ~ poly(reserves, 12, raw=TRUE))
    slope <- model.matrix(fit_value) %*% matrix(deriv_coef(fit_value), ncol=1)
    res_vals<- reserves
    der_func <<- lm(slope ~ poly(res_vals, 12, raw=TRUE))
    #solve it
    test_soln<-nleqslv(x_test,cons_ext_full,jacobian=NULL,control=list(maxit=500),res_pass=reserves)
    #qcheck it
    if(test_soln$termcd>1){ #resolve it
      #print(paste("resolving, Iteration",iter,"Flag is",test_soln$convergence,collapse = " "))
      print(paste("resolving, Iteration",iter,"Flag is",test_soln$termcd,collapse = " "))
      lambda <- numeric(NROW(reserves))+12
      res_ext <- pmin((reserves*.25),45)
      ghgs <- res_ext*eibbls
      x_test <- c(res_ext,lambda,ghgs)
      test_soln<-nleqslv(x_test,cons_ext_full,jacobian=NULL,control=list(maxit=500),res_pass=reserves)
      #print(paste("resolved, Iteration",iter,"New flag is",test_soln$convergence,collapse = " "))
      print(paste("resolved, Iteration",iter,"New Flag is",test_soln$termcd,collapse = " "))
    }
    #pars<-test_soln$par
    pars<-test_soln$x
    rows_soln <- length(pars)/3
    opt_ext <- pars[1:rows_soln]
    if(iter==1)
      init_ext<-opt_ext
    #mtext("Optimal Extraction", side=4, line=3)
    x_2 <- pars[(rows_soln+1):(rows_soln*2)]
    ghgs <- pars[(rows_soln*2+1):(rows_soln*3)]
    #lambda <- numeric(NROW(reserves))+oil_price/10
    next_ext<-opt_ext
    next_ext[reserves<100]<-opt_ext[reserves<100]*.9
    next_lambda<-x_2
    next_lambda[reserves<100]<-x_2[reserves<100]*1.1
    next_ghgs<-ghgs
    next_ghgs[reserves<100]<-ghgs[reserves<100]*.9
    x_test <- c(next_ext,next_lambda,next_ghgs) #set starting values for next iteration
    
    #x_test <- c(opt_ext-1,(x_2))
    #netback_h <- (1-net_rev_royalty(oil_price))*((1-gross_rev_royalty(oil_price))*(oil_price-cons_carb_tax*eibbls*incidence)*opt_ext-tot_ext_cost(ghgs,opt_ext,reserves,abate_curve))
    #netback_h <- (1)*((1-gross_rev_royalty(oil_price))*(oil_price-cons_carb_tax*eibbls*incidence)*opt_ext-tot_ext_cost(ghgs,opt_ext,reserves,abate_curve))
    net_rev<-((1-gross_rev_royalty(oil_price))*(oil_price-cons_carb_tax*eibbls*incidence)*opt_ext-tot_ext_cost(ghgs,opt_ext,reserves,abate_curve))
    prod_oil_val<-(oil_price-cons_carb_tax*eibbls*incidence)*opt_ext
    oil_rev<-oil_price*opt_ext
    cons_oil_val<-(oil_price+cons_carb_tax*eibbls*(1-incidence))*opt_ext
    #lines(reserves,net_rev,col="red",type="l")
    value_old <- value_h
    endog_reserves<-reserves-opt_ext
    value_h<- (1-net_rev_royalty(oil_price))*net_rev+.95*ipolate(endog_reserves)
    crit <- mean((value_old-value_h)^2)
    
    if((iter-1)%%5==0)
    {    
      par(mfrow=c(2,1))
      par(mar=c(2.5,4,2,4))
      y_range<-range(0,100)
      x_range<-range(reserves)
      plot(x_range,y_range, type = "n")
      lines(reserves,tot_ext_cost(ghgs,opt_ext,reserves,abate_curve)/opt_ext,col="black")
      lines(reserves,reserves*0+oil_price, col="red")
      lines(reserves,marg_ext_cost(ghgs,opt_ext,reserves,abate_curve), col="blue")
      plot(reserves,opt_ext*eibbls,col="blue")
      lines(reserves,ghgs,col="black",type="l")
      
      par(mfrow=c(2,1))
      par(mar=c(2.5,4,2,4))
      plot(reserves,sim_der(reserves),col="blue")
      #axis(side=2, at = pretty(range(m_value(reserves))))
      par(new = TRUE)
      y_range<-range(opt_ext,init_ext)
      x_range<-range(reserves)
      plot(x_range,y_range, type = "n",xaxt="n",yaxt="n", bty = "n", xlab = "", ylab = "",lwd=4,cex.lab=.8, cex.axis=.8, cex.main=.8, cex.sub=.8)
      lines(reserves,opt_ext, col="red", type = "l", bty = "n", xlab = "", ylab = "",lwd=4)
      lines(reserves,init_ext, col="black", lty=2,type = "l",bty = "n", xlab = "", ylab = "",lwd=1)
      axis(side=4, at = pretty(range(opt_ext)))
      legend_vec<-vector()
      col_vec<-vector()
      y_range<-range(net_rev,cons_oil_val,oil_rev)#all extraction columns
      x_range<-range(reserves)
      legend_vec<-vector()
      plot(x_range,y_range,type="n",lwd=2,xlab="Reserves (Millions of Barrels)",ylab="Revenues and Netbacks (millions of dollars per year)",cex.lab=.8, cex.axis=.8, cex.main=.8, cex.sub=.8)
      lines(reserves,(1-net_rev_royalty(oil_price))*net_rev,col="black",type="l")
      legend_vec<-cbind(legend_vec,c("Net Revenue"))
      col_vec<-cbind(col_vec,c("black"))
      lines(reserves,net_rev,col="red",type="l")
      legend_vec<-cbind(legend_vec,c("Rev after gross rev roy"))
      col_vec<-cbind(col_vec,c("red"))
      lines(reserves,prod_oil_val,col="blue",type="l",lwd=4)
      legend_vec<-cbind(legend_vec,c("Prod oil val"))
      col_vec<-cbind(col_vec,c("blue"))
      lines(reserves,oil_rev,col="green",type="l")
      legend_vec<-cbind(legend_vec,c("Oil price * Quantity"))
      col_vec<-cbind(col_vec,c("green"))
      lines(reserves,cons_oil_val,col="orange",type="l")
      legend_vec<-cbind(legend_vec,c("Consumer oil Value"))
      col_vec<-cbind(col_vec,c("orange"))
      #layout(rbind(1,2), heights=c(7,1))  # put legend on bottom 1/8th of the chart
      legend("bottomright",legend_vec,ncol=3,lty=c(1,1),lwd=c(1,1,1,1,1),col=col_vec,cex=.8)
      
      #y_range<-range(opt_ext[0:20])
      #x_range<-range(reserves[0:20])
      #plot(x_range, y_range, main = "Constrained Extraction",type="n")
      #lines(reserves[0:20],opt_ext[0:20],col="red",lwd=4,lty=1)
      #lines(reserves[0:20],init_ext[0:20],col="blue",lwd=2,lty=1)
      #legend("topleft",c("Optimal Extraction","Initial Extraction"),lty=c(1,1),col=c("red","blue"))
      print(c(iter,crit))
    }
    iter <- iter + 1
    #crit<-0
  }
  
  output_frame<-data.frame(cbind(reserves,opt_ext,ghgs,x_2,value_h,sim_der(reserves)))
  names(output_frame) <- c(paste("reserves",deparse(scn),sep="_"), paste("opt_ext",deparse(scn),sep="_"), paste("ghgs",deparse(scn),sep="_"), paste("lambda",deparse(scn),sep="_"), paste("value",deparse(scn),sep="_"),paste("marg_val",deparse(scn),sep="_"))
  return(output_frame)  
}


#solve for optimal resources for a group of scenarios

opt_res_solution<-function(s)
{
  #testing s<-4
  col_name<-paste("marg_val",deparse(s),sep="_")
  res_tax_val<- ifelse(scenarios[[s,"Tax Type"]]=="reserves",scenarios[[s,"Carbon Tax"]],0)
  ipl_mv_res <- splinefun(reserves,out_data[[col_name]],method = "natural")
  ipl_mc_res <- splinefun(reserves,marg_reserve_cost(reserves,res_tax_val),method = "natural")
  nle_res_dyn <- function(x){
    return((ipl_mc_res(x)-ipl_mv_res(x))[1][1])}
  num_test<-180
  opt_res<- nleqslv(num_test,nle_res_dyn,jacobian=NULL)
  opt_res$x
}


opt_resource_graphs<-function(svec,file_sent="test.png",width_sent=12,height_sent=6,file_exp=0,nudge_y_sent=0,nudge_x_sent=0,
                              line_vec_sent=rep("solid",10),col_vec_sent=grey.colors(10),verb_string=0,no_ticks=0,
                              line_vec_opt=rep("solid",10),col_vec_opt=grey.colors(10)){
  #svec is a set of scenarios
  #svec<-c(1,2,3)
  #svec<-select_data
  #want this to return optimal reserves for each one, and graph the outcomes?
  
  col_names<-paste("opt_ext",svec,sep="_")
  plot_data<-tibble(reserves) %>% cbind(out_data%>%select(all_of(col_names)))%>%
    pivot_longer(-reserves,names_to = "scenario",values_to = "extraction") %>%
    mutate(scenario=as.numeric(gsub("opt_ext_","",scenario)))%>%
    left_join(scenarios %>% select(scenario=Number,string))%>%
    mutate(scenario=factor(scenario))%>%
    mutate(string=ifelse(string=="base case","Base case optimal decision rules",paste("Optimal decision rules with",string)),
           string=(str_wrap(string,30))
    )
  
  ext_plot<-ggplot(plot_data)+
    geom_line(aes(reserves,extraction,group=string,col=string,lty=string),size=rel(1.25))+
    scale_color_manual("",values=col_vec_opt)+
    scale_linetype_manual("",values=line_vec_opt)+
    labs(x="Reserves (Millions of Barrels)",y="Optimal Resource Extraction (millions of bbls per year)")+
    theme_classic()+
    guides(color=guide_legend(ncol=2,byrow=FALSE,keywidth = 6),lty=guide_legend(nrow=2,byrow=FALSE,keywidth = 6))+
    theme(
      #legend.position=c(.25,.9),
      text = element_text(size = 14),
      legend.position = "bottom",
    )
  
  if(no_ticks==1){
    ext_plot<-ext_plot+theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
      axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))
    )
  }
  
  col_names<-paste("ghgs",svec,sep="_")
  plot_data<-tibble(reserves) %>% cbind(out_data%>%select(all_of(col_names)))%>%
    pivot_longer(-reserves,names_to = "scenario",values_to = "ghgs")%>%
    mutate(scenario=as.numeric(gsub("ghgs_","",scenario)))%>%
    left_join(scenarios %>% select(scenario=Number,string))%>%
    mutate(scenario=factor(scenario))%>%
    mutate(string=ifelse(string=="base case","Base case optimal decision rules",paste("Optimal decision rules with",string)),
           string=(str_wrap(string,30))
    )
  
  
  ghg_plot<-ggplot(plot_data)+
    geom_line(aes(reserves,ghgs,group=string,col=string,lty=string),size=rel(1.25))+
    scale_color_manual("",values=col_vec_opt)+
    scale_linetype_manual("",values=line_vec_opt)+
    labs(x="Reserves (Millions of Barrels)",y="Optimal GHG Emissions (millions of tonnes per year)")+
    theme_classic()+
    guides(color=guide_legend(ncol=3,byrow=FALSE,keywidth = 6),lty=guide_legend(nrow=2,byrow=FALSE,keywidth = 6))+
    theme(
      #legend.position=c(.25,.9),
      text = element_text(size = 14),
      legend.position = "bottom",
    )
  if(no_ticks==1){
    ghg_plot<-ghg_plot+theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
      axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))
    )
  }  
  
  legend <- get_legend(
    # create some space to the left of the legend
    ghg_plot + guides(color = guide_legend(ncol=3,keywidth = 5),
                      lty = guide_legend(ncol=3,keywidth = 5)
    ) + 
      theme(legend.position = "bottom")
  )
  
  prow<-plot_grid(ext_plot+ theme(legend.position="none"),
                  ghg_plot + theme(legend.position="none"),
                  hjust = -1,
                  nrow = 1,
                  rel_widths = c(1, 1)
  )
  
  # now add in the legend
  plot_grid(prow, legend, ncol = 1, rel_heights = c(1, .2))
  
  if(file_exp==1)
    ggsave(file=paste("opt_combo_",file_sent,sep=""), width = width_sent, height = height_sent,dpi = 300)
  
  
  #second graph 
  
  soln_data<-tibble(s=svec)%>% group_by(s) %>% mutate(res_tax_val= ifelse(scenarios[[s,"Tax Type"]]=="reserves",scenarios[[s,"Carbon Tax"]],0))%>%
    mutate(col_name=paste("marg_val",deparse(s),sep="_"),
           mv_func=list(splinefun(reserves,out_data[[col_name]],method = "natural")),
           mc_func=list(splinefun(reserves,marg_reserve_cost(reserves,res_tax_val),method = "natural")))%>%
    mutate(opt_res=opt_res_solution(s))%>% group_by(s)%>%
    mutate(mv_imp=mv_func[[1]](opt_res),
           mc_imp=mc_func[[1]](opt_res))%>%
    left_join(scenarios %>% select(s=Number,string,mc_string,mv_string))%>%
    mutate(string=if_else(string=="base case",str_to_sentence(paste(string,"equilibrium")),str_to_sentence(paste("Equilibrium with",string))),
           mc_string=if_else(mc_string=="base case",str_to_sentence(paste(mc_string,"marginal extraction cost")),str_to_sentence(paste("Marginal extraction cost with",mc_string))),
           mv_string=if_else(mv_string=="base case",str_to_sentence(paste(mv_string,"marginal indirect value")),str_to_sentence(paste("Marginal indirect value with",mv_string))),
    )%>%
    mutate(mc_string=str_wrap(mc_string,30),mv_string=str_wrap(mv_string,30))
  
  #ggplot(soln_data)+
  #  geom_line(aes(x=seq(0,200),y=mv_func[[1]](seq(0,200))))
  
  #hacking this because we did it stupidly before
  
  col_names<-paste("marg_val",svec,sep="_")
  
  values_data<- out_data%>%select(reserves,all_of(col_names))%>%
    pivot_longer(-reserves,names_to = "scenario",values_to = "marg_val")%>%
    mutate(s=as.double(gsub("marg_val_","",scenario)))%>%select(-scenario) %>%
    left_join(soln_data%>%select(s,mv_func,mc_func,res_tax_val,string,mc_string,mv_string))%>% group_by(s) %>%
    mutate(mv_imp=mv_func[[1]](reserves),
           mc_imp=mc_func[[1]](reserves),
           marg_cost=marg_reserve_cost(reserves,res_tax_val))%>%
    select(-mc_func,-mv_func) %>% rename(scenario=s)%>%
    mutate(scenario=as.factor(scenario))%>% ungroup()
  
  #verb_string<-1  
  soln_data<-soln_data %>% mutate(string=ifelse(verb_string==1,paste(str_wrap(string,15),"\n(",round(opt_res,2),", ",round(mv_imp,2),")",sep=""),str_wrap(string,15)))
  #nudge_x_sent<-1
  #nudge_y_sent<-2
  equil_plot<-ggplot(values_data)+
    geom_line(aes(reserves,mc_imp,color=mc_string,lty=mc_string),size=rel(1.25))+
    geom_line(aes(reserves,mv_imp,color=mv_string,lty=mv_string),size=rel(1.25))+
    scale_linetype_manual("",values=line_vec_sent)+
    scale_color_manual("",values=col_vec_sent)+
    scale_x_continuous(breaks = pretty_breaks(),expand=c(0,0))+
    scale_y_continuous(breaks = pretty_breaks(),expand=c(0,0))+
    geom_point(data = soln_data,aes(opt_res,mv_imp),size=4)+
    geom_text_repel(data = soln_data,aes(x=opt_res,y=mv_imp,label=string),size=3,nudge_y = nudge_y_sent,nudge_x = nudge_x_sent)+
    labs(x="Developed barrels of proved reserves (millions)",y="Marginal cost or indirect value per barrel of reserves created ($/bbl)")+
    theme_classic()+
    guides(color=guide_legend(nrow=2,byrow=FALSE,keywidth = 6),lty=guide_legend(nrow=2,byrow=FALSE,keywidth = 6))+
    theme(
      #legend.position=c(.25,.9),
      text = element_text(size = 14),
      legend.position = "bottom",
    )
  if(no_ticks==1){
    equil_plot<-equil_plot+theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
      axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))
    )
  }
  
  #equil_plot
  if(file_exp==1)
    ggsave(equil_plot,file=paste("equil_",file_sent,sep = ""),dpi=300,width = width_sent,height = height_sent)
}

#simulate optimal extraction over time from optimal reserves

simulate<-function(simul_per,s,opt_res_sent){ #simulate the extraction path for a scenario s and opt_res_sent
  #opt_res_sent<-350
  #s<-8
  assign_globals(scenarios[s,]) #use the scenario database to assign the globals
  print(paste("Scenario",s,scenario_string(s),sep = " ")  )
  sim_store<-tibble()  #storage object to store this simulation for scenario s 
  start_res<-opt_res_sent
  if(start_res>0)
  {
    sim_res<-start_res[1]
    col_name<-paste("marg_val",deparse(s),sep="_")
    #establish derivative approximation using marginal values from model solutions
    mvals<-out_data[[col_name]]
    res_vals<-out_data$reserves
    # approximate the derivative with a higher-order polynomial
    der_func <<- lm(mvals ~ poly(res_vals, 10, raw=TRUE))
    sim_der<-function(res_data){predict(der_func,data.frame(res_vals=res_data))}
    starting_controls<-c(.5*sim_res,oil_price/8,8)
    for(per_t in 1:simul_per) {
      #print(paste("time period",per_t,sep=" "))
      #print(test_cont)
      nleq_soln <- nleqslv(starting_controls,cons_ext_full,jacobian=NULL,method="Newton",control=list(maxit=1500),res_pass=sim_res)
      if(nleq_soln$termcd>1)
      {
        print(paste("time period",per_t,", flag=",nleq_soln$termcd," trying a reset",sep=""))
        nleq_soln2 <- nleqslv(c(sim_res*.05,sim_res*.02,sim_res*.02*eibbls),cons_ext_full,jacobian=NULL,method="Newton",control=list(maxit=1500),res_pass=sim_res)
        if(nleq_soln2$termcd==1){
          nleg_soln<-nleq_soln2
          print("reset successful")
        }
      }
      sim_ext <- nleq_soln$x[1]
      sim_lambda <- nleq_soln$x[2]
      sim_ghgs<-nleq_soln$x[3]
      sim_store<-sim_store %>% bind_rows(tibble(sim_res,sim_ext,sim_ghgs))
      sim_res<-max(sim_res-sim_ext,.1) #keep reserves positive for the solution to work
      test_cont<-c(sim_ext*.5,18,sim_ghgs*.5)
      if(sim_res<sim_ext)
        starting_controls<-c(min(sim_res*.05,sim_ext*.5),18,sim_ghgs*.05)
    }
  }
  else
    print("starting reserves not positive")
  names(sim_store) <- c("reserves","opt_ext","GHGs")
  #print(sim_store)
  sim_store <-sim_store %>% mutate(time=row_number(),scenario=s) %>% relocate(time)
  sim_store
}


Simfig_combo<-function(svec,sims_sent,file_sent,width_sent=12,height_sent=6,file_exp=0,legend_sent=FALSE,
                       col_vec_sent=grey.colors(10),line_vec_sent=rep("solid",10),no_ticks=0){
  #get sim_data for sims in svec
  #get legend strings from the scenarios global data
  #svec<-c(1,2,3)
  #
  sims_sent<-sims_sent %>% filter(scenario %in% svec)
  
  
  #get scenario strings
  sims_sent<-sims_sent %>% left_join(scenarios %>% select(scenario=Number,string))%>%
    mutate(scenario=factor(scenario))%>%
    mutate(string=ifelse(string=="base case","Base case simulation from optimal reserves",paste("Simulation from optimal reserves with",string)),
           string=(str_wrap(string,40))
    )
  
  ext_plot<-ggplot(sims_sent)+
    geom_line(aes(time,opt_ext,group=scenario,col=string,lty=string),size=rel(1.25))+
    scale_color_manual("",values=col_vec_sent)+
    scale_linetype_manual("",values=line_vec_sent)+
    scale_x_continuous(breaks=pretty_breaks(),expand=c(0,0))+
    scale_y_continuous(breaks=pretty_breaks(),expand=c(0,0))+
    labs(x="Time",y="Optimal Resource Extraction (millions of bbls per year)")+
    theme_classic()+
    guides(color=guide_legend(ncol=3,byrow=FALSE,keywidth = 6),lty=guide_legend(nrow=2,byrow=FALSE,keywidth = 6))+
    theme(
      #legend.position=c(.25,.9),
      text = element_text(size = 14),
      legend.position = "bottom",
    )
  ext_plot
  if(no_ticks==1){
    ext_plot<-ext_plot+theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
      axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))
    )
  }
  
  
  ghg_plot<-ggplot(sims_sent)+
    geom_line(aes(time,GHGs,group=scenario,col=string,lty=string),size=rel(1.25))+
    scale_color_manual("",values=col_vec_sent)+
    scale_linetype_manual("",values=line_vec_sent)+
    scale_x_continuous(breaks=pretty_breaks(),expand=c(0,0))+
    scale_y_continuous(breaks=pretty_breaks(),expand=c(0,0))+
    labs(x="Time",y="Optimal GHG Emissions (millions of tonnes per year)")+
    theme_classic()+
    guides(color=guide_legend(ncol=3,byrow=FALSE,keywidth = 6),lty=guide_legend(nrow=2,byrow=FALSE,keywidth = 6))+
    theme(
      #legend.position=c(.25,.9),
      text = element_text(size = 14),
      legend.position = "bottom",
    )
  ghg_plot
  
  if(no_ticks==1){
    ghg_plot<-ghg_plot+theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
      axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))
    )
  }
  
  
  legend <- get_legend(
    # create some space to the left of the legend
    ghg_plot + guides(color = guide_legend(ncol = 3,keywidth = 5),
                      lty = guide_legend(ncol=3,keywidth = 5)) + 
      theme(legend.position = "bottom"
      )
  )
  
  prow<-plot_grid(ext_plot+ theme(legend.position="none"),
                  ghg_plot + theme(legend.position="none"),
                  hjust = -1,
                  nrow = 1,
                  rel_widths = c(1, 1)
  )
  
  # now add in the legend
  plot_grid(prow, legend, ncol = 1, rel_heights = c(1, .2))
  
  if(file_exp==1)
    ggsave(file=paste("sim_combo_",file_sent,sep=""), width = width_sent, height = height_sent,dpi = 300)
  # print(legend_vec)
}


assign_globals<-function(policy_vec)
{
  oil_price<<-45
  oil_price<<-as.numeric(policy_vec$`Oil Price`)
  price_vec<-c(oil_price)  
  res_carb_tax<<-0
  ext_carb_tax<<-0
  oba_val<<-0
  cons_carb_tax<<-0 #impact of consumption carbon taxes on oil prices
  if(policy_vec$`Tax Type`=="extraction"){
    ext_carb_tax<<-policy_vec$`Carbon Tax`
    oba_val<<-policy_vec$`Carbon Tax`
    #print(paste("OBA_val is ",oba_val))
  }
  if(policy_vec$`Tax Type`=="consumption")
    cons_carb_tax<<-policy_vec$`Carbon Tax`
  if(policy_vec$`Tax Type`=="reserves"){
    res_carb_tax<<-policy_vec$`Carbon Tax`
    oba_val<<-policy_vec$`Carbon Tax`
    #print(paste("OBA_val is ",oba_val))
  }
  net_rev_rate<<-0
  gross_rev_rate<<-0
  if(policy_vec$Base =="gross")
    gross_rev_rate<<-policy_vec$`Royalty`
  if(policy_vec$Base =="net")
    net_rev_rate<<-policy_vec$`Royalty`
  oba_rate<<-policy_vec$`OBA`
  abate_curve<<-policy_vec$`MAC_TYPE`
}



#run from here to solve the model fully

starting_vals<-0
select_data<-scenarios[[1]] #all scenarios in a numeric vector
out_data<-data.frame(reserves)

#select_data<-scenarios
#print(colnames(scenarios))
for(scn in select_data){
  #solve_data<-model_solve(p,carb_vec)
  print(paste("Scenario ",scn))
  oil_price<-scenarios[scn,grep("Oil Price", colnames(scenarios))]
  price_vec=c(oil_price)
  print(" ")
  policy_vec<-data.frame(scenarios[scn,])
  names(policy_vec)<-colnames(scenarios)
  assign_globals(policy_vec)
  print(paste("oil price should be",scenarios$`Oil Price`[scn],"and is",oil_price))
  print(paste("carbon price should be",scenarios$`Carbon Tax`[scn],"applied at",scenarios$`Tax Type`[scn]))
  print(paste("carbon prices are",res_carb_tax,ext_carb_tax,cons_carb_tax))
  print(paste("Royalty rate should be",scenarios$Royalty[scn],"applied against",scenarios$Base[scn]))
  print(paste("Royalty rates are",gross_rev_rate,net_rev_rate))
  print(paste("Abatement curves should be",scenarios$MAC_TYPE[scn]," and is ",abate_curve))
  
  start.time <- Sys.time()
  if(starting_vals==1)
  {
    start_vals<-start_data[,scn]
    solve_data<-model_solve(value_sent=start_vals)
  }
  else
    solve_data<-model_solve()
  out_data<-cbind(out_data,solve_data)
  end.time <- Sys.time()
  time.taken<-round(as.period(end.time-start.time, unit = "minutes"),digits=0)
  print(paste("Time to solve this iteration",time.taken))
}


time_periods<-60
select_data<-scenarios[[1]] #run all scenarios
sim_data<-tibble()
force_labels<-0
for(scn in select_data){
  print(scn) #scn<-1
  sim_data<-sim_data %>% bind_rows(simulate(time_periods,scn,opt_res_solution(scn)))#simulate based on scenario numbers and time periods.
}


filename<-paste("all_solve_",format(Sys.Date(),"%Y_%m_%d"),".RData",sep = "")

save(list = c("sim_data","out_data"), file= filename)

#or run from here loading the data
#filename<-paste("all_2018-04-01.RData",sep = "")
#load(file=filename)


  #fix everything but oil and carbon
  sim_id<-"oil_carbon"
  select_data<-scenarios[which(scenarios$`Oil Price` == 80 &scenarios$`Tax Type` =="extraction" & scenarios$Royalty ==0 & scenarios$`Base`=="gross" & scenarios$OBA ==0 & scenarios$MAC_TYPE =="abate_base"),1]
  select_data<-select_data[[1]]
  opt_resource_graphs(select_data,paste(sim_id,".png",sep=""),width_sent=12,height_sent = 7.25,
                      nudge_y_sent = c(15,20),
                      nudge_x_sent = c(0,0),
                      
                      col_vec_sent = c("black","black","grey40","grey70","grey50"),
                      line_vec_sent = c("solid","11","33","13","31"),
                      #base concave conves
                      col_vec_opt = c("black","grey40","grey70","grey50"),
                      line_vec_opt = c("solid","33","13","31"),
                      
                      
                      file_exp=1,no_ticks = 1)
  
  Simfig_combo(select_data,sim_data,file_sent =paste(sim_id,".png",sep=""),
               file_exp = 1,line_vec_sent = c("solid","33","13","31"),
               col_vec_sent = c("black","grey40","grey70","grey50"),no_ticks = 1)
  
  
  
  #royalty rate impacts
  sim_id<-"royalties"
  select_data<-scenarios %>% filter(`Oil Price` == 80,`Tax Type` =="extraction",`Carbon Tax` ==0,scenarios$OBA ==0 ,scenarios$MAC_TYPE =="abate_base")%>%
    select(Number)
  #select_data<-select_data[-1]
  select_data<-select_data[[1]]
  opt_resource_graphs(select_data,paste(sim_id,".png",sep=""),width_sent=12,height_sent = 7.25,
                      nudge_y_sent = c(25,30,20),
                      nudge_x_sent = c(0,-50,50),
                      
                      col_vec_sent = c("black","black","grey40","grey70","grey50"),
                      line_vec_sent = c("solid","11","33","13","31"),
                      #base concave conves
                      col_vec_opt = c("black","grey40","grey70","grey50"),
                      line_vec_opt = c("solid","33","13","31"),
                      
                      
                      file_exp=1,no_ticks = 1)
  
  Simfig_combo(select_data,sim_data,file_sent =paste(sim_id,".png",sep=""),
               file_exp = 1,line_vec_sent = c("solid","33","13","31"),
               col_vec_sent = c("black","grey40","grey70","grey50"),no_ticks = 1)
  
  
  sim_id<-"royalties_2"
  select_data<-c(1,2,5,6)
  opt_resource_graphs(select_data,paste(sim_id,".png",sep=""),width_sent=12,height_sent = 7.5,
                      nudge_y_sent = c(25,30,32,20),
                      nudge_x_sent = c(0,0,-50,0),
                      
                      col_vec_sent = c("black","black","grey40","grey70","grey50"),
                      line_vec_sent = c("solid","11","33","13","31"),
                      #base concave conves
                      col_vec_opt = c("black","grey40","grey70","grey50"),
                      line_vec_opt = c("solid","33","13","31"),
                      
                      
                      file_exp=1,no_ticks = 1)
  
  Simfig_combo(select_data,sim_data,file_sent =paste(sim_id,".png",sep=""),
               file_exp = 1,line_vec_sent = c("solid","33","13","31"),
               col_vec_sent = c("black","grey40","grey70","grey50"),no_ticks = 1)
  
  
  sim_id<-"OBAs"
  force_labels<-1
  select_data<-scenarios[which(scenarios$`Oil Price` == 80 & scenarios$Royalty ==0 & scenarios$`Base`=="gross" & scenarios$`Tax Type` =="extraction" & scenarios$MAC_TYPE =="abate_base"),1]
  select_data<-select_data[[1]]
  opt_resource_graphs(select_data,paste(sim_id,".png",sep=""),width_sent=12,height_sent = 7.25,
                      nudge_y_sent = c(15,20,10),
                      nudge_x_sent = c(0,0,-100),
                      
                      col_vec_sent = c("black","black","grey40","grey70","grey50"),
                      line_vec_sent = c("solid","11","33","13","31"),
                      #base concave conves
                      col_vec_opt = c("black","grey40","grey70","grey50"),
                      line_vec_opt = c("solid","33","13","31"),
                      
                      
                      file_exp=1,no_ticks = 1)
  
  Simfig_combo(select_data,sim_data,file_sent =paste(sim_id,".png",sep=""),
               file_exp = 1,line_vec_sent = c("solid","33","13","31"),
               col_vec_sent = c("black","grey40","grey70","grey50"),no_ticks = 1)
  
  
  sim_id<-"res_tax"
  select_data<-scenarios[which(scenarios$`Oil Price` == 80 & scenarios$Royalty ==0 & scenarios$`Base`=="gross" & scenarios$OBA ==0 & scenarios$MAC_TYPE =="abate_base"),1]
  select_data<-select_data[[1]]
  
  opt_resource_graphs(select_data,paste(sim_id,".png",sep=""),width_sent=12,height_sent = 7.25,
                      nudge_y_sent = c(15,20,23.5),
                      nudge_x_sent = c(0,0,0),
                      
                      
                      col_vec_sent = c("black","black","grey70","grey40","grey50"),
                      line_vec_sent = c("solid","11","13","33","31"),
                      #base concave conves
                      col_vec_opt = c("black","grey40","grey70","grey50"),
                      line_vec_opt = c("solid","33","13","31"),
                      
                      
                      file_exp=1,no_ticks = 1)
  
  Simfig_combo(select_data,sim_data,file_sent =paste(sim_id,".png",sep=""),
               file_exp = 1,line_vec_sent = c("solid","33","13","31"),
               col_vec_sent = c("black","grey40","grey70","grey50"),no_ticks = 1,height_sent = 6.25)
  
  
  


sim_id<-"MACs"

force_labels<-1
select_data<-scenarios[which(scenarios$`Oil Price` == 80 & scenarios$Royalty ==0 & scenarios$`Base`=="gross" & scenarios$`Tax Type` =="extraction" & scenarios$OBA ==0),1]
select_data<-select_data[[1]]
select_data<-c(1,2,7,8)

opt_resource_graphs(select_data,paste(sim_id,".png",sep=""),width_sent=12,height_sent = 7.65,
                    nudge_y_sent = c(10,30,23.5,20),
                    nudge_x_sent = c(25,0,100,-100),
                    
                    col_vec_sent = c("black","black","grey40","grey70","grey50"),
                    line_vec_sent = c("solid","11","33","13","31"),
                    #base concave conves
                    col_vec_opt = c("black","grey40","grey70","black","grey50"),
                    line_vec_opt = c("solid","33","13","31"),
                    
                    
                    file_exp=1,no_ticks = 1)

Simfig_combo(select_data,sim_data,file_sent =paste(sim_id,".png",sep=""),
             file_exp = 1,line_vec_sent = c("solid","33","13","31"),
             col_vec_sent = c("black","grey40","grey70","grey50"),no_ticks = 1,height_sent = 6.25)




sim_id<-"scenario_test"
select_data<-c(1,2,3,4)
opt_resource_graphs(select_data,paste(sim_id,".png",sep=""),width_sent=12,height_sent = 7.5,
                    nudge_y_sent = c(25,30,40,20),
                    nudge_x_sent = c(0,0,0,0),
                    col_vec_sent = c("black","black","grey40","black","black"),
                    line_vec_sent = c("solid","11","33","solid","31"),
                    file_exp=1,no_ticks = 1)

Simfig_combo(select_data,sim_data,file_sent =paste(sim_id,".png",sep=""),
             file_exp = 1,line_vec_sent = c("solid","11","41","12"),no_ticks = 1)

