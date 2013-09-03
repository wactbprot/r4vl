
pltfill <- ggplot(ndfill)
pltfill <- pltfill + geom_point(aes(x = Mpkt,
                                    y = value,
                                    color = factor(ndfill$variable)),
                                size=5)
pltfill <- pltfill +    guides(color = guide_legend("Pressure"))
pltfill <- pltfill +    scale_y_log10()
pltfill <- pltfill +    theme(legend.position="bottom")

if(withU){
    pltfill <- pltfill +  geom_errorbar(aes(x=Mpkt,
                                            ymax = pfill*(1 + upfill),
                                            ymin = pfill*(1 - upfill)))
}



pltfilloffs <- ggplot(ndffilloffs)
pltfilloffs <- pltfilloffs +   geom_point(aes(x = Mpkt,
                                              y = value,
                                              color = factor(ndffilloffs$variable)),
                                          size=5)
pltfilloffs <- pltfilloffs +     guides(color = guide_legend("Pressure"))
pltfilloffs <- pltfilloffs +     theme(legend.position="bottom")
