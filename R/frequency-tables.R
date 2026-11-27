# Tables follow the dataset level order, including retained unused categories.
.saraFrequencyRows <- function(column, cumulative=FALSE, odds=FALSE, numericValues=FALSE) {
    if (numericValues) {
        column <- .saraNumeric(column)
        observed <- sort(unique(column[!is.na(column)]))
        # Count original numbers, not formatted labels or lexicographic categories.
        codes <- match(column, observed)
        labels <- as.character(observed)
        if (anyDuplicated(labels))
            labels <- format(observed, digits=17, trim=TRUE)
    } else {
        if (!is.factor(column)) column <- factor(column)
        labels <- levels(column)
        codes <- as.integer(column)
    }
    counts <- as.integer(tabulate(codes, nbins=length(labels)))
    n <- sum(counts)
    prop <- if (n > 0L) counts/n else rep(NA_real_, length(counts))
    values <- data.frame(levels=c(labels, "Missing"),
                         counts=c(counts, sum(is.na(codes))),
                         pc=c(prop, NA_real_),
                         stringsAsFactors=FALSE, check.names=FALSE)
    if (cumulative) {
        values$cumcounts <- c(cumsum(counts), NA_real_)
        values$cumpc <- c(if (n > 0L) cumsum(counts)/n else rep(NA_real_, length(counts)), NA_real_)
        values <- values[c("levels", "counts", "cumcounts", "pc", "cumpc")]
    }
    if (odds) values$odds <- c(if (n > 0L) counts/(n-counts) else rep(NA_real_, length(counts)), NA_real_)
    values
}

.saraInitFrequencyTables <- function(tables, data, vars, cumulative=FALSE, odds=FALSE, numericValues=FALSE, translate=identity) {
    for (var in vars) {
        table <- tables$get(var)
        # .init may receive only the header. Category rows are added in .run.
        columns <- list(levels=c(if (numericValues) "Values" else "Levels", "text"), counts=c("Counts", "integer"))
        if (cumulative) columns$cumcounts <- c("Cumulative counts", "integer")
        columns$pc <- c("% of Total", "number")
        if (cumulative) columns$cumpc <- c("Cumulative %", "number")
        if (odds) columns$odds <- c("Odds", "number")
        for (name in names(columns)) {
            spec <- columns[[name]]
            table$addColumn(name=name, title=translate(spec[1]), type=spec[2],
                            format=if (name %in% c("pc", "cumpc")) "pc" else "")
        }
    }
}

.saraPopulateFrequencyTables <- function(tables, data, vars, cumulative=FALSE, odds=FALSE, numericValues=FALSE, translate=identity) {
    for (var in vars) {
        table <- tables$get(var)
        values <- .saraFrequencyRows(data[[var]], cumulative, odds, numericValues)
        values$levels[nrow(values)] <- translate("Missing")
        table$deleteRows()
        # Positional keys distinguish a category literally named Missing from missing data.
        for (i in seq_len(nrow(values))) {
            key <- as.character(i)
            table$addRow(rowKey=key, values=list())
            table$setRow(rowNo=i, values=as.list(values[i,,drop=FALSE]))
        }
        table$setNote("valid", if (numericValues)
            translate("Percentages use non-missing observations. Cumulative values follow ascending numeric order and exclude missing observations.")
            else if (cumulative)
            translate("Percentages use non-missing observations. Cumulative values follow the defined category order and exclude missing observations.")
            else translate("Percentages and odds use non-missing observations. Odds are infinite when all valid observations belong to one category."))
    }
}

# Header-safe detection used only to retain the existing ordinal Range behaviour.
.saraFrequencyHasNumeric <- function(data, vars) {
    any(vapply(vars, function(var) {
        x <- data[[var]]
        stored <- attr(x, "values", exact=TRUE)
        if (!is.null(stored) && is.numeric(stored)) return(TRUE)
        is.numeric(tryCatch(.saraNumeric(x), error=function(e) NULL))
    }, logical(1)))
}
