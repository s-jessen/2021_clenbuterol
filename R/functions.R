barplot <- function(df, variable, ylab, ylim = NULL) {


    #Calculate visually pleasing y-axis limits w. padding for p-values
    y_values <- df %>% filter(time != "pre") %>% pull(!!sym(variable))

    y_breaks <- scales::pretty_breaks(n = 5)(y_values)

    y_min <- min(y_breaks)

    y_max <- max(y_breaks*1.2)

    #Plot figure
    fig <- df %>%
        dplyr::filter(time != "pre") %>%
        ggplot2::ggplot(aes(x = treatment, y = !!sym(variable), fill = treatment))+
        geom_bar(
            stat = "summary",
            fun = "mean",
            color = "black",
            width = 0.6)+
        geom_point(size = 2,
                   shape = 16,
                   position = position_jitter(width = 0.05),
                   color = "black",
                   alpha = 0.5)+
        geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.25)+
        theme(
            panel.background = element_blank(),
            panel.border = element_rect(colour = "black", fill=NA, linewidth = 0.25),
            panel.grid.minor=element_blank(),
            panel.grid.major = element_blank(),
            plot.background = element_blank(),
            axis.line = element_blank(),
            axis.text = element_text(color = "black", size = 10),
            text = element_text(size = 10, family="Source Sans Pro", color = "black"),
            axis.title = element_text(size = 10, family="Source Sans Pro"),
            legend.position = "none",
            legend.title = element_blank(),
            legend.key = element_blank(),
            legend.key.size = (unit(3, "mm")),
            legend.background = element_blank(),
            legend.box.background = element_blank(),
            title = element_text(size = 6),
            strip.background = element_blank()
        )+
        scale_fill_manual(
            values = c("PLA" = pla_color, "CLEN" = clen_color)
        )+
        labs(x = "", y = ylab)+
        scale_y_continuous(limits = c(y_min, y_max), breaks = y_breaks)

    print(fig)

}
