#@author: miguel.guevara@postgrado.usm.cl
#@lastupdate: 19-Jun-13
#@description: a fuzyy model for users in Twitter in the context of political elections
#dataset means the dataset to be used, and verbose if you want to print the process of defuzzyfication
library("fields") #for color scales and other

getIndexVotationg <- function(dataset="synt", verbose=0)
{
  #Fuzzy model
  library("sets") 
  ## set up fuzzy variables 
  source("singleton.r")
  #fuzzy_singleton <- charfun_generator(singleton)

  if(dataset == 'synt')
  {
    #sintetic
    q=10
    alpha=1.3
    max=60
    experiment<-list('1'=matrix(c(5,4,1,5,0,4),nrow=2,ncol=3, byrow=T)) #1
    candidate1<- "Candidate1"
    candidate2<- "Candidate2"
  }
  
  if(dataset == 'stgo')
  {
    #stgo
    q=10 #punto quiebre donde distribucion ya no es power law
    alpha= 1.3771644
    max=61
    candidate1<- "Toha"
    candidate2 <- "Zalaquette"
    #stgo12 #cada item de lista es un usuario, 
    experiment<-list('1'=matrix(c(5,1,0,5,0,3),nrow=2,ncol=3, byrow=T), '2'=matrix(c(61,3,16,61,33,1),nrow=2,ncol=3,byrow=T)) #1
  }
  
  #us
  if(dataset == 'us')
  {
    q=15 #punto quiebre donde distribucion ya no es power law
    alpha = 1.32615733
    max= 70
    candidate1 <- "Obama"
    candidate2 <- "Romney"
    #us2012 ids, 112294503, 260327851, 392941727
    experiment<-list('1'=matrix(c(14,0,5,14,4,0),nrow=2,ncol=3,byrow=T), '2'=matrix(c(4,2,0,4,2,1),nrow=2,ncol=3,byrow=T), '3'=matrix(c(40,23,4,40,2,17),nrow=2,ncol=3,byrow=T))
  }
    
  #cdp
  if(dataset == 'cdp')
  {
    q=6 #punto quiebre donde distribucion ya no es power law
    alpha = 1.463150
    max= 36
    candidate1 <- "Rincon"
    candidate2 <- "Orrego"
    #cdp13 ids 520529996 (orrego), 444600256 (ximena2014)
    experiment<-list('1'=matrix(c(17,0,0,17,11,0),nrow=2,ncol=3,byrow=T), '2'=matrix(c(17,2,0,17,0,0),nrow=2,ncol=3,byrow=T)) #1

  }    
  ## set universe 
  #max<- 40
  min<- 0
 sets_options("universe", seq(from = -4, to = max, by = 0.1))
 # sets_options("universe", NULL)
  #datos calculados
  l=c(min,1,floor(q/(alpha)^3),ceiling(q/alpha^2))
  m=c(l[[3]],l[[4]], q, floor(q*alpha))
  h=c(m[[3]], m[[4]], ceiling(q*alpha^2),max^3)  #i fix last value like square max, for graphic proposes, it should be max, just max
  hp=c(0,q,max,max^2)
  hn=c(0,q,max,max^2) 
  #radio = floor((max/6)/2)  #radio para triangulares para crisp output variable
  radio <- 0.5
  
  #datos fijos
  #l=c(min,1,5,8)
  #m=c(6,10,14,18)
  #h=c(14,18,max,max*100)
  #hp=c()
  #hn=c()
    
    
  variables <-
    set( userParticipation = fuzzy_variable(low = fuzzy_trapezoid(corners =l), medium = fuzzy_trapezoid(corners = m), high= fuzzy_trapezoid(corners = h)),
           PositivePS = fuzzy_variable(highP = fuzzy_trapezoid(corners = hp)),
          NegativePS = fuzzy_variable(highN = fuzzy_trapezoid(corners =hn)),    
         #votingIntention = fuzzy_partition(varnames=(c(Att=-2,Agn=-1,Ntr=0, Vtr=1, Prm=2, Spm = 3)), FUN = fuzzy_singleton, Height=1)
         votingIntention = fuzzy_partition(varnames = c(Att = -2, Agn = -1, Ntr = 0, Vtr=1, Prm = 2, Spm = 3),   FUN = fuzzy_cone, radius = 0.5)
         #votingIntentionD = fuzzy_partition(varnames=(c(Att=-2,Agn=-1,Ntr=0, Vtr=1, Prm=2, Spm = 3)), FUN = fuzzy_singleton, Height=1)
    )
   #         votingIntention = fuzzy_variable(Att = singleton(center=-2, height=1), Agn = singleton(center=-1,height=1), Ntr = singleton(center=-0,height=1), Vtr=singleton(center=1,height=1), Prm = singleton(center=2,height=1), Spm = singleton(center=3,height=1)))
          # votingIntention = fuzzy_partition(varnames=(c(Att=-2,Agn=-1,Ntr=0, Vtr=1, Prm=2, Spm = 3)), FUN = fuzzy_singleton, Height=1)

  #todo cambiar voting_variable por votingPARTITIONB
  #par(mfrow=c(1,3))
  #plot(variables)
  plot(variables$userParticipation, xlab="Number of Tweets",lwd=1)
  
  axis(side=1,at=c(l,m,h))
  lines(c(l[2],l[2]),c(1,0),lty=2)
  lines(c(l[3],l[3]),c(1,0),lty=2)
  lines(c(m[2],m[2]),c(1,0),lty=2)
  lines(c(m[3],m[3]),c(1,0),lty=2)
  lines(c(h[2],h[2]),c(1,0),lty=2)
  
  par(mfrow=c(1,2))
  # guardar imagen?dev.copy2eps()
  
  plot(variables$PositivePS, xlab="Number of Positive Tweets",lwd=2)
 
  plot(variables$NegativePS, xlab="Number of Negative Tweets",lwd=2)
  
  
  ## set up rules 
  rules <- 
    set( 
      fuzzy_rule(userParticipation %is% low && PositivePS %is% highP, votingIntention %is% Vtr),
      fuzzy_rule(userParticipation %is% medium && PositivePS %is% highP, votingIntention %is% Prm),
      fuzzy_rule(userParticipation %is% high && PositivePS %is% highP, votingIntention %is% Spm),
      fuzzy_rule(userParticipation %is% low && NegativePS %is% highN, votingIntention %is% Agn),
      fuzzy_rule(userParticipation %is% medium && NegativePS %is% highN, votingIntention %is% Agn),
      fuzzy_rule(userParticipation %is% high && NegativePS %is% highN, votingIntention %is% Att),
      fuzzy_rule(userParticipation %is% low && PositivePS %is% highP && NegativePS %is% highN, votingIntention %is% Ntr),
      fuzzy_rule(userParticipation %is% medium && PositivePS %is% highP && NegativePS %is% highN, votingIntention %is% Ntr),
      fuzzy_rule(userParticipation %is% high && PositivePS %is% highP && NegativePS %is% highN, votingIntention %is% Ntr)
    
      
      
      
    ) 
  #negativo implica, siendo un low frequency user, que tira para el neutro.... IMPORTANTE,
  
  ## combine to a system
  system <- fuzzy_system(variables, rules)
  show(system)
  print(system)
  plot(system) ## plots variables

  ## do inference
  par(mfrow=c(1,2))
  i<-1
  for(exp in experiment)
  {
    tupla<- exp[1,]
    n<-tupla[1];pos<-tupla[2];neg<-tupla[3];
    
    fi <- fuzzy_inference(system, list(userParticipation = n, PositivePS = pos, NegativePS=neg))
    
    #str(fi)
    attY<- round(gset_memberships(fi,filter=-2),1)
    agnY<- round(gset_memberships(fi,filter=-1),1)
    ntrY<- round(gset_memberships(fi,filter=0),1)
    vtrY<- round(gset_memberships(fi,filter=1),1)
    prmY<- round(gset_memberships(fi,filter=2),1)
    spmY<- round(gset_memberships(fi,filter=3),1)
    
    if(is.empty(attY)){attY<-0};if(is.empty(agnY)){agnY<-0};if(is.empty(ntrY)){ntrY<-0};
    if(is.empty(vtrY)){vtrY<-0};if(is.empty(prmY)){prmY<-0};if(is.empty(spmY)){spmY<-0};
    fiY<-c(attY,agnY,ntrY,vtrY,prmY,spmY)  #results for ordered axis=MASS
    print(fiY)
    str(fiY)
    massByArm= (fiY[1]*1)+(fiY[2]*2)+(fiY[3]*3)+(fiY[4]*4)+(fiY[5]*5)+(fiY[6]*6) #using reference 0 on -1 just for comodity
    fivi <-(massByArm/sum(fiY)) - 3 # -3 corrects the reference used to do the calc of the arm
    str(massByArm)
    results<-paste('massByArm',massByArm,'FIVI', fivi, 'FIVITrunc',trunc(fivi), sep=' ')
    print(results)
    
    if(verbose==1)
    {  
      #PLOTEAR PROCESO Fuzzy TODAS REGLAS
      #POSITIVOS
      par(mfrow=c(1,3))
      plot(variables$userParticipation, xlab="Number of Tweets",lwd=1)
      lines(c(n,n),c(1,0),lty=2, col='red')
      plot(variables$PositivePS, xlab="Number of Positive Tweets",lwd=1)
      lines(c(pos,pos),c(1,0),lty=2, col='red')
      plot(c(-2.5:3.5), c(0,0,1,1,1,1,1), , type='n', ylab='',xlab='', main=paste(dataset, i, candidate1, sep=" "))
      #escala colores
      #image.plot( zlim=c(-2.5,3.5), legend.only=TRUE, legend.width=2,legend.shrink=.92, horizontal=TRUE, col=gray(c(280:0 / 280, 0:200 / 200)))
      #lines(variables$votingIntentionD, col="black")
      lines(c(-2,-2),c(0,1));lines(c(-1,-1),c(0,1));lines(c(0,0),c(0,1));lines(c(1,1),c(0,1));lines(c(2,2),c(0,1));lines(c(3,3),c(0,1))
      text(c(-2),c(0.4), 'Att');text(c(-1),c(0.4), 'Agn');text(c(0),c(0.4), 'Ntr');text(c(1),c(0.4), 'Vtr');text(c(2),c(0.4), 'Prm');text(c(3),c(0.4), 'Spm')
      
      
      #NEGATIVOS
      plot(variables$userParticipation, xlab="Number of Tweets",lwd=1);box(which="inner")
      lines(c(n,n),c(1,0),lty=2, col='red')
      plot(variables$NegativePS, xlab="Number of Negative Tweets",lwd=1);box(which="inner")
      lines(c(neg,neg),c(1,0),lty=2, col='red')
      plot(c(-2.5:3.5), c(0,0,1,1,1,1,1), , type='n', ylab='',xlab='', main=paste(dataset, i, candidate1, sep=" "))
      #escala colores
      #image.plot( zlim=c(-2.5,3.5), legend.only=TRUE, legend.width=2,legend.shrink=.92, horizontal=TRUE, col=gray(c(280:0 / 280, 0:200 / 200)))
      #lines(variables$votingIntentionD, col="black")
      lines(c(-2,-2),c(0,1));lines(c(-1,-1),c(0,1));lines(c(0,0),c(0,1));lines(c(1,1),c(0,1));lines(c(2,2),c(0,1));lines(c(3,3),c(0,1))
      text(c(-2),c(0.4), 'Att');text(c(-1),c(0.4), 'Agn');text(c(0),c(0.4), 'Ntr');text(c(1),c(0.4), 'Vtr');text(c(2),c(0.4), 'Prm');text(c(3),c(0.4), 'Spm')
      
    }  
    #RESULTADO POLIGONO
    par(mfrow=c(1,2))
    
    #para hacer un plot a escala mejor
    plot(c(-2.5:3.5), c(0,0,1,1,1,1,1), , type='n', ylab='',xlab='', main=paste(dataset, i, candidate1, sep=" "))
    #escala colores
    image.plot( zlim=c(-2.5,3.5), legend.only=TRUE, legend.width=2,legend.shrink=.92, horizontal=TRUE, col=gray(c(280:0 / 280, 0:200 / 200)))
    #lines(variables$votingIntention, col="black")
    #pintar el area de pertenencia
    #polygon(c(min(fi), fi, max(fi)), c(min(gset_memberships(fi)), gset_memberships(fi), min(gset_memberships(fi))),col='gray')
    colRef<-'gray'
    lines(c(-2,-2),c(0,1),col=colRef);lines(c(-1,-1),c(0,1),col=colRef);lines(c(0,0),c(0,1),col=colRef);lines(c(1,1),c(0,1),col=colRef);lines(c(2,2),c(0,1),col=colRef);lines(c(3,3),c(0,1),col=colRef)
    widthResult<-4
    textDesp<-0.25
    if(fiY[1] != 0) lines(c(-2,-2),c(0,fiY[1]),lwd=widthResult);  if(fiY[2] != 0) lines(c(-1,-1),c(0,fiY[2]),lwd=widthResult);
    if(fiY[3] != 0) lines(c(0,0),c(0,fiY[3]),lwd=widthResult); if(fiY[4] != 0) lines(c(1,1),c(0,fiY[4]),lwd=widthResult);
    if(fiY[5] != 0) lines(c(2,2),c(0,fiY[5]),lwd=widthResult); if(fiY[6] != 0) lines(c(3,3),c(0,fiY[6]),lwd=widthResult)
    text(c(-2+textDesp),c(0.4), 'Att');text(c(-1+textDesp),c(0.4), 'Agn');text(c(0+textDesp),c(0.4), 'Ntr');text(c(1+textDesp),c(0.4), 'Vtr');text(c(2+textDesp),c(0.4), 'Prm');text(c(3+textDesp),c(0.4), 'Spm')
    ## defuzzify mamdany
    index<-0
    index<-gset_defuzzify(fi, "centroid")
    #points(index, 0, pch=13, cex=3, col="red")
    
    #sugeno results
    points(fivi, 0, pch=13, cex=3, col="gray")
    points(trunc(fivi), 0, pch=13, cex=3, col="black")
    
    #kk<-gset_defuzzify(fi, "largestofmax")
    resLatex <- paste(n, pos, neg, round(index, digits=2), "C", sep=" & ")

    sets_options("universe", NULL)
    
    #respecto segundo candidato************************
    tupla<- exp[2,]
    n<-tupla[1];pos<-tupla[2];neg<-tupla[3];
    
    fi <- fuzzy_inference(system, list(userParticipation = n, PositivePS = pos, NegativePS=neg))
    
    #str(fi)
    attY<- round(gset_memberships(fi,filter=-2),1)
    agnY<- round(gset_memberships(fi,filter=-1),1)
    ntrY<- round(gset_memberships(fi,filter=0),1)
    vtrY<- round(gset_memberships(fi,filter=1),1)
    prmY<- round(gset_memberships(fi,filter=2),1)
    spmY<- round(gset_memberships(fi,filter=3),1)
    
    
    if(is.empty(attY)){attY<-0};if(is.empty(agnY)){agnY<-0};if(is.empty(ntrY)){ntrY<-0};
    if(is.empty(vtrY)){vtrY<-0};if(is.empty(prmY)){prmY<-0};if(is.empty(spmY)){spmY<-0};
    fiY<-c(attY,agnY,ntrY,vtrY,prmY,spmY)  #results for ordered axis=MASS
    print(fiY)
    str(fiY)
    massByArm= (fiY[1]*1)+(fiY[2]*2)+(fiY[3]*3)+(fiY[4]*4)+(fiY[5]*5)+(fiY[6]*6) #using reference 0 on -1 just for comodity
    fivi <-(massByArm/sum(fiY)) - 3 # -3 corrects the reference used to do the calc of the arm
    str(massByArm)
    results<-paste('massByArm',massByArm,'FIVI', fivi, 'FIVITrunc',trunc(fivi), sep=' ')
    print(results)
    
    if(verbose==1)
    {
        #PLOTEAR PROCESO Fuzzy TODAS REGLAS
        par(mfrow=c(1,3))
        plot(variables$userParticipation, xlab="Number of Tweets",lwd=1);box(which="inner")
        lines(c(n,n),c(1,0),lty=2, col='red')
        plot(variables$PositivePS, xlab="Number of Positive Tweets",lwd=2);box(which="inner")
        lines(c(pos,pos),c(1,0),lty=2, col='red')
        plot(c(-2.5:3.5), c(0,0,1,1,1,1,1), , type='n', ylab='',xlab='', main=paste(dataset, i, candidate2, sep=" "))
        #escala colores
        #image.plot( zlim=c(-2.5,3.5), legend.only=TRUE, legend.width=2,legend.shrink=.92, horizontal=TRUE, col=gray(c(280:0 / 280, 0:200 / 200)))
        #lines(variables$votingIntentionD, col="black")
        lines(c(-2,-2),c(0,1));lines(c(-1,-1),c(0,1));lines(c(0,0),c(0,1));lines(c(1,1),c(0,1));lines(c(2,2),c(0,1));lines(c(3,3),c(0,1))
        text(c(-2),c(0.4), 'Att');text(c(-1),c(0.4), 'Agn');text(c(0),c(0.4), 'Ntr');text(c(1),c(0.4), 'Vtr');text(c(2),c(0.4), 'Prm');text(c(3),c(0.4), 'Spm')
        
        
        plot(variables$userParticipation, xlab="Number of Tweets",lwd=1)
        lines(c(n,n),c(1,0),lty=2, col='red')
        plot(variables$NegativePS, xlab="Number of Negative Tweets",lwd=2)
        lines(c(neg,neg),c(1,0),lty=2, col='red')
        plot(c(-2.5:3.5), c(0,0,1,1,1,1,1), , type='n', ylab='',xlab='', main=paste(dataset, i, candidate2, sep=" "))
        #escala colores
        #image.plot( zlim=c(-2.5,3.5), legend.only=TRUE, legend.width=2,legend.shrink=.92, horizontal=TRUE, col=gray(c(280:0 / 280, 0:200 / 200)))
       # lines(variables$votingIntentionD, col="black")
        lines(c(-2,-2),c(0,1));lines(c(-1,-1),c(0,1));lines(c(0,0),c(0,1));lines(c(1,1),c(0,1));lines(c(2,2),c(0,1));lines(c(3,3),c(0,1))
        
        text(c(-2),c(0.4), 'Att');text(c(-1),c(0.4), 'Agn');text(c(0),c(0.4), 'Ntr');text(c(1),c(0.4), 'Vtr');text(c(2),c(0.4), 'Prm');text(c(3),c(0.4), 'Spm')   
    }  
    
    #RESULTADOS POLIGONO SEGUNDO CANDIDATO*******************************
    
    #para hacer un plot a escala mejor
    plot(c(-2.5:3.5), c(0,0,1,1,1,1,1), , type='n', ylab='',xlab='', main=paste(dataset, i, candidate2, sep=" "))
    #escala colores
    image.plot( zlim=c(-2.5,3.5), legend.only=TRUE, legend.width=2,legend.shrink=.92, horizontal=TRUE, col=gray(c(280:0 / 280, 0:200 / 200)))
    #lines(variables$votingIntention, col="black")
    #pintar el area de pertenencia
    #polygon(c(min(fi), fi, max(fi)), c(min(gset_memberships(fi)), gset_memberships(fi), min(gset_memberships(fi))),col='gray')
    colRef<-'gray'
    lines(c(-2,-2),c(0,1),col=colRef);lines(c(-1,-1),c(0,1),col=colRef);lines(c(0,0),c(0,1),col=colRef);lines(c(1,1),c(0,1),col=colRef);lines(c(2,2),c(0,1),col=colRef);lines(c(3,3),c(0,1),col=colRef)
    widthResult<-4
    textDesp<-0.25
    if(fiY[1] != 0) lines(c(-2,-2),c(0,fiY[1]),lwd=widthResult);  if(fiY[2] != 0) lines(c(-1,-1),c(0,fiY[2]),lwd=widthResult);
    if(fiY[3] != 0) lines(c(0,0),c(0,fiY[3]),lwd=widthResult); if(fiY[4] != 0) lines(c(1,1),c(0,fiY[4]),lwd=widthResult);
    if(fiY[5] != 0) lines(c(2,2),c(0,fiY[5]),lwd=widthResult); if(fiY[6] != 0) lines(c(3,3),c(0,fiY[6]),lwd=widthResult)
    text(c(-2+textDesp),c(0.4), 'Att');text(c(-1+textDesp),c(0.4), 'Agn');text(c(0+textDesp),c(0.4), 'Ntr');text(c(1+textDesp),c(0.4), 'Vtr');text(c(2+textDesp),c(0.4), 'Prm');text(c(3+textDesp),c(0.4), 'Spm')
    ## defuzzify mamdany
    index<-0
    index<-gset_defuzzify(fi, "centroid")
    #points(index, 0, pch=13, cex=3, col="red")
    
    #sugeno results
    points(fivi, 0, pch=13, cex=3, col="gray")
    points(trunc(fivi), 0, pch=13, cex=3, col="black")
    
    resLatex <- paste(resLatex, pos, neg, round(index,digits=2), "C", sep=" & ")
    print(resLatex)
    
    i<- i +1
  }
}

is.empty <- function(x, mode=NULL){
  if (is.null(mode)) mode <- class(x)
  identical(vector(mode,1),c(x,vector(class(x),1)))
}
