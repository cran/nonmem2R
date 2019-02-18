####################################################################################################
#' Internal package function
#'
#' @param vpc.results
#' vpc results
#' @param npc.results
#' npc results
#' @param verbose
#' verbose
#' @param ...
#' Further arguments
#' @return
#' Named list
#' @importFrom utils read.table
#' @export
#' @keywords internal
readVpc<-function (vpc.results = NULL, npc.results = NULL, verbose = FALSE,...)
{
    if (is.null(vpc.results) & is.null(npc.results)) {
        cat(paste("Both the arguments vpc.results and npc.results are NULL\n"))
        cat(paste("One of these must be defined\n"))
        return(NULL)
    }
    if (!is.null(vpc.results) & !is.null(npc.results)) {
        cat(paste("Both the arguments vpc.results and npc.results are defined\n"))
        cat(paste("ONLY one of these may be defined\n"))
        return(NULL)
    }
    vpc <- FALSE
    npc <- FALSE
    if (!is.null(vpc.results))
        vpc <- TRUE
    if (!is.null(npc.results))
        npc <- TRUE
    if (vpc)
        filename <- vpc.results
    if (npc)
        filename <- npc.results
    if (file.exists(filename)[1]) {
        if (verbose)
            cat(paste("    Reading", filename, "\n"))
        scannedFile <- scan(filename, sep = "\n", what = character(),
            quiet = TRUE, blank.lines.skip = FALSE)
    }
#    else {
#        cat(paste(filename, "was not found for reading\n"))
#        return(NULL)
#    }
    blank.line.pat <- "^$"
    if (vpc) {
        table.start.pat <- "VPC results"
        table.head.pat <- "<="
    }
    if (npc) {
        table.start.pat <- "NPC results"
        table.head.pat <- "points below PI"
    }
    table.start <- grep(table.start.pat, scannedFile)
    num.tables <- length(table.start)
    table.head <- grep(table.head.pat, scannedFile)
    blank.line <- grep(blank.line.pat, scannedFile)
    table.stop <- c()
    for (i in 1:num.tables) {
        for (j in 1:length(blank.line)) {
            if (table.start[i] > blank.line[j])
                next
            if (table.start[i] < blank.line[j]) {
                table.stop <- c(table.stop, blank.line[j] - 1)
                break
            }
        }
    }
    table.rows.to.read <- table.stop - table.head
    dv.pat <- "Dependent variable"
    idv.pat <- "Independent variable"
    mod.pat <- "Modelfile"
    dv.idv.table.start <- grep(dv.pat, scannedFile)
    dv.idv.table.stop <- NULL
    for (j in 1:length(blank.line)) {
        if (dv.idv.table.start > blank.line[j])
            next
        if (dv.idv.table.start < blank.line[j]) {
            dv.idv.table.stop <- blank.line[j] - 1
            break
        }
    }
    dv.idv.table <- read.table(filename, skip = dv.idv.table.start -
        1, nrows = dv.idv.table.stop - dv.idv.table.start, sep = ",",
        comment.char = "", header = T, strip.white = TRUE)
    dv.var <- paste(dv.idv.table[[grep("Dependent.variable",
        names(dv.idv.table))]])
    model.file <- paste(dv.idv.table[[grep("Modelfile", names(dv.idv.table))]])
    if (vpc)
        idv.var <- paste(dv.idv.table[[grep("Independent.variable",
            names(dv.idv.table))]])
    cat.tables <- F
    cen.tables <- F
    cat.boundaries <- NULL
    lloq <- NA
    uloq <- NA
    pred.corr <- FALSE
    var.corr <- FALSE
    add.feats.row <- grep("Additional.feature", scannedFile)
    if (length(add.feats.row) != 0) {
        for (i in 1:length(add.feats.row)) {
            if (length(grep("Categorization", scannedFile[add.feats.row[i] +
                1])) != 0) {
                cat.tables <- T
                boundary.table <- read.table(filename, skip = add.feats.row[i] +
                  1, nrows = 1, sep = ",", fill = T, comment.char = "",
                  strip.white = TRUE, header = T)
                boundary.rows <- grep("Boundary", names(boundary.table))
                cat.boundaries <- boundary.table[, boundary.rows]
            }
            if (length(grep("Censored.data", scannedFile[add.feats.row[i] +
                1])) != 0) {
                cen.tables <- T
                censored.table <- read.table(filename, skip = add.feats.row[i] +
                  1, nrows = 1, sep = ",", fill = T, comment.char = "",
                  strip.white = TRUE, header = T)
                lloq <- censored.table$LLOQ
                uloq <- censored.table$ULOQ
            }
            if (length(grep("Prediction.correction", scannedFile[add.feats.row[i] +
                1])) != 0) {
                pred.corr <- T
            }
            if (length(grep("Variability.correction", scannedFile[add.feats.row[i] +
                1])) != 0) {
                var.corr <- T
            }
        }
    }
    by.interval <- NULL
    strata.names <- NULL
    if (num.tables > 1) {
        bin.table <- vector("list", num.tables + 1)
        strata.names <- c()
        tmp.interval <- c()
        for (i in 1:num.tables) {
            strata.pat <- "strata"
            strata.line <- scannedFile[table.start[i]]
            strata.start <- regexpr(strata.pat, strata.line) +  7
            if (strata.start == 6) {
                tmp.strata <- NULL
                tmp.interval <- NULL
            }
            else {
                strata.stop <- regexpr(",", strata.line) - 1
                if (strata.stop == -2)
                  strata.stop <- regexpr("$", strata.line)
                tmp.strata <- substring(strata.line, strata.start,
                  strata.stop)
                strata.var.stop <- regexpr(" ", tmp.strata) -
                  1
                strata.int.start <- regexpr(" ", tmp.strata) +
                  1
                strata.var <- substring(tmp.strata, 1, strata.var.stop)
                strata.int <- substring(tmp.strata, strata.int.start)
                tmp.strata = gsub("\\[", ">= ", tmp.strata)
                tmp.strata = gsub("\\]", paste(" >=", strata.var,
                  sep = " "), tmp.strata)
                tmp.strata = gsub("\\(", "> ", tmp.strata)
                tmp.strata = gsub("\\)", paste(" >", strata.var,
                  sep = " "), tmp.strata)
                tmp.strata = gsub("\\;", " \\& ", tmp.strata)
                tmp.strata = gsub(" = ", " == ", tmp.strata)
            }
            strata.names <- c(strata.names, tmp.strata)
            if (!is.null(tmp.strata)) {
                if (regexpr(">", tmp.strata) != -1) {
                  semi.loc <- regexpr("\\;", strata.int)
                  lt.GE.loc <- regexpr("\\[", strata.int)
                  lt.GT.loc <- regexpr("\\(", strata.int)
                  rt.LE.loc <- regexpr("\\]", strata.int)
                  rt.LT.loc <- regexpr("\\)", strata.int)
                  strata.int.low <- substring(strata.int, 1,
                    semi.loc - 1)
                  strata.int.low <- gsub("\\[", "", strata.int.low)
                  strata.int.low <- gsub("\\(", "", strata.int.low)
                  strata.int.low <- gsub(" ", "", strata.int.low)
                  strata.int.low <- as.numeric(strata.int.low)
                  strata.int.high <- substring(strata.int, semi.loc +
                    1)
                  strata.int.high <- gsub("\\]", "", strata.int.high)
                  strata.int.high <- gsub("\\)", "", strata.int.high)
                  strata.int.high <- gsub(" ", "", strata.int.high)
                  strata.int.high <- as.numeric(strata.int.high)
                  interval.length <- strata.int.high - strata.int.low
                  add.to.ends <- interval.length * 1e-07
                  if (lt.GT.loc != -1)
                    strata.int.low <- strata.int.low + add.to.ends
                  if (rt.LT.loc != -1)
                    strata.int.high <- strata.int.high - add.to.ends
                  tmp.interval <- c(tmp.interval, strata.int.low,
                    strata.int.high)
                }
            }
            bin.table[[i]] <- read.table(filename, skip = table.head[i] -
                1, nrows = table.rows.to.read[i], sep = ",",
                comment.char = "", header = T, strip.white = TRUE,
                blank.lines.skip = FALSE)
			####
			#AAAA<-read.table(filename, skip = table.head[i] -
            #    1, nrows = table.rows.to.read[i], sep = ",",
            #    comment.char = "", header = T, strip.white = TRUE,
            #    blank.lines.skip = FALSE)[,1:5]
			#print(AAAA)	
			####
        }
        if (length(tmp.interval) != 0) {
            by.interval <- matrix(tmp.interval, nrow = num.tables,
                ncol = 2, byrow = T)
        }
        bin.table[[num.tables + 1]] <- strata.names
    }
    else {
        bin.table <- read.table(filename, skip = table.head -
            1, nrows = table.rows.to.read, sep = ",", comment.char = "",
            header = T, strip.white = TRUE, blank.lines.skip = FALSE)
    }
    for (i in 1:num.tables) {
        if (num.tables == 1) {
            tmp.table <- bin.table
        }
        else {
            tmp.table <- bin.table[[i]]
        }
        if (vpc) {
            tmp.table$X <- NULL
            tmp.table$X.1 <- NULL
            names(tmp.table)[1] <- "lower"
            names(tmp.table)[2] <- "upper"
            names(tmp.table)[3] <- "nobs"
        }
        if (npc) {
            names(tmp.table)[1] <- "PI"
            tmp.table$PI <- as.numeric(sub("% PI", "", tmp.table$PI))
        }
        tmp.names <- names(tmp.table)
        tmp.names <- sub("X\\.*", "", tmp.names)
        tmp.names <- gsub("_", ".", tmp.names)
        tmp.names <- gsub("\\.+", "\\.", tmp.names)
        tmp.names <- gsub("\\.$", "", tmp.names)
        names(tmp.table) <- tmp.names
		###print(tmp.names) ->  "lower"               "upper"               "nobs"                "no.of.obs"
		###print(tmp.table) : third column (nobs) is mean of idv
        if (num.tables == 1) {
            bin.table <- tmp.table
        }
        else {
            bin.table[[i]] <- tmp.table
        }
    }
    table.multiples = 1
    if (cat.tables)
        table.multiples = table.multiples + 1
    if (cen.tables)
        table.multiples = table.multiples + 1
    if (table.multiples > 1) {
        bin.table.cont <- vector("list", num.tables/table.multiples)
        if (cat.tables)
            bin.table.cat <- vector("list", num.tables/table.multiples)
        if (cen.tables)
            bin.table.cen <- vector("list", num.tables/table.multiples)
        sub.i <- 0
        for (ii in seq(1, num.tables, by = table.multiples)) {
            sub.i <- sub.i + 1
            bin.table.cont[[sub.i]] <- bin.table[[ii]]
        }
        if (sub.i == 1)
            bin.table.cont <- bin.table.cont[[sub.i]]
        cen.start = 2
        if (table.multiples == 3) {
            cat.start = 3
        }
        else {
            cat.start = 2
        }
        if (cen.tables) {
            sub.i <- 0
            for (ii in seq(cen.start, num.tables, by = table.multiples)) {
                sub.i <- sub.i + 1
                bin.table.cen[[sub.i]] <- bin.table[[ii]]
            }
            if (sub.i == 1)
                bin.table.cen <- bin.table.cen[[sub.i]]
        }
        else {
            bin.table.cen <- NULL
        }
        if (cat.tables) {
            sub.i <- 0
            for (ii in seq(cat.start, num.tables, by = table.multiples)) {
                sub.i <- sub.i + 1
                bin.table.cat[[sub.i]] <- bin.table[[ii]]
            }
            if (sub.i == 1)
                bin.table.cat <- bin.table.cat[[sub.i]]
        }
        else {
            bin.table.cat <- NULL
        }
        if (cat.tables) {
            num.tables.cat <- num.tables/table.multiples
        }
        else {
            num.tables.cat <- NULL
        }
        if (cen.tables) {
            num.tables.cen <- num.tables/table.multiples
        }
        else {
            num.tables.cen <- NULL
        }
        num.tables.cont <- num.tables/table.multiples
        strata.names <- strata.names[seq(1, num.tables, by = table.multiples)]
    }
    else {
        bin.table.cont <- bin.table
        bin.table.cat <- NULL
        bin.table.cen <- NULL
        num.tables.cont <- num.tables
        num.tables.cat <- NULL
        num.tables.cen <- NULL
    }
    if (npc)
        return(list(model.file = model.file, dv.var = dv.var,
            idv.var = NULL, num.tables = num.tables, result.tables = bin.table))
    if (vpc)
        return(list(model.file = model.file, dv.var = dv.var,
            idv.var = idv.var, num.tables = num.tables.cont,
            by.interval = by.interval, result.tables = bin.table.cont,
            strata.names = strata.names, num.tables.cat = num.tables.cat,
            result.tables.cat = bin.table.cat, cat.boundaries = cat.boundaries,
            num.tables.cen = num.tables.cen, result.tables.cen = bin.table.cen,
            lloq = lloq, uloq = uloq, pred.corr = pred.corr,
            var.corr = var.corr))
}

