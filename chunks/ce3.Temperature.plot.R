ggplot(ndf) + 
    geom_point(aes(x = Mpkt, y = value, color = factor(ndf$variable)), size=5) + 	  
    guides(color = guide_legend("Temperatures")) +
    geom_hline(yintercept = upperLimit, lwd = 1, linetype = "dotted", color="red") + 
    geom_hline(yintercept = lowerLimit, lwd = 1, linetype = "dotted", color="red" ) + 
    geom_hline(yintercept = aim, lwd = 1, linetype =
               "longdash",color="darkgreen") +
    geom_errorbar(aes(x=Mpkt,ymax = Tfm3*(1 + uTfm), ymin=Tfm3*(1 -
                                                         uTfm)), width=0.1) +
    theme(legend.position="bottom")
