# Render explanatory panels instead of leaving an image unfinished when
# standardisation is impossible. Invalid groups do not hide valid groups.
.saraNormalityPlot <- function(data, variable, groupBy=NULL, kind="qq",
                              ggtheme=NULL, translate=identity) {
    values <- jmvcore::toNumeric(data[[variable]])
    groups <- if (is.null(groupBy)) rep("All",length(values)) else as.character(data[[groupBy]])
    groupNames <- if (is.null(groupBy)) "All" else unique(groups[!is.na(groups)])
    messages <- lapply(groupNames, function(group) {
        x <- values[!is.na(groups) & groups==group & is.finite(values)]
        reason <- if (length(x)<2L) "At least two finite observations are required." else
            if (!is.finite(stats::sd(x)) || stats::sd(x)==0)
                "The variable is constant or its standard deviation is not finite." else NULL
        if (is.null(reason)) return(NULL)
        data.frame(group=group,x=0,y=0,label=paste(
            translate(if (kind=="qq") "Q-Q plot unavailable." else "Standardized ECDF unavailable."),
            paste(strwrap(translate(reason),width=42),collapse="\n"),sep="\n"))
    })
    notes <- do.call(rbind,messages)
    frame <- .saraECDFData(values,if (is.null(groupBy)) NULL else groups)
    usable <- !is.null(frame) && nrow(frame)>0L
    if (usable) {
        if (kind=="qq") {
            plot <- .saraQQPlot(data,variable,groupBy,ggtheme)
        } else {
            plot <- ggplot2::ggplot(frame,ggplot2::aes(x=.data$z)) +
                ggplot2::geom_line(ggplot2::aes(y=.data$normal),colour="#7b9ee6") +
                ggplot2::geom_step(ggplot2::aes(y=.data$empirical)) +
                ggplot2::geom_point(ggplot2::aes(y=.data$empirical)) +
                ggplot2::labs(x=translate("Standardized values"),
                              y=translate("Cumulative probability")) + ggtheme
        }
    } else {
        plot <- ggplot2::ggplot() + ggplot2::theme_void()
        if (is.null(notes)) notes <- data.frame(group="All",x=0,y=0,
            label=translate("No observations with valid group membership are available."))
    }
    if (!is.null(notes) && usable) {
        # Centre text in the actual shared scales, rather than at (0,0).
        # In an ECDF, zero lies at the bottom and clips multiline labels.
        horizontal <- if (kind=="qq") plot$data$theoretical else frame$z
        vertical <- if (kind=="qq") c(plot$data$lower,plot$data$upper,
                                      plot$data$sample) else c(0,1)
        notes$x <- mean(range(horizontal[is.finite(horizontal)]))
        notes$y <- mean(range(vertical[is.finite(vertical)]))
    }
    if (!is.null(notes)) plot <- plot + ggplot2::geom_text(data=notes,
        mapping=ggplot2::aes(x=.data$x,y=.data$y,label=.data$label),
        inherit.aes=FALSE,size=3.5)
    if (!is.null(groupBy)) {
        # Explanatory panels need enough width at jamovi's fixed image size.
        # Keep the usual layout when all groups have valid plots.
        columns <- if (!is.null(notes)) 1L else NULL
        plot <- plot + ggplot2::facet_wrap(~group,ncol=columns)
    }
    plot
}
