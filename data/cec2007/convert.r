##
## convert.r - convert CEC2007 result text files into an R data.frame
##
## Author:
##   Olaf Mersmann <olafm@statistik.tu-dortmund.de>
##

parseHeader <- function(line) {
  l <- tolower(line)
  l <- gsub(" +$", "", l)
  if (regexpr("[0-9]\\.", l) > 0) {
    l <- gsub(" +[0-9]+\\. *", ";", l)
    l <- gsub(" +1.", ";", l)
  } else { ## LYT:
    l <- gsub(" +", ";", l)
  }
  l <- gsub(" ", "_", l)
  l <- gsub("oka1", "oka2", l) ## Error in zielinsik* :/
  header <- unlist(strsplit(l, ";"))
  if (header[1] != "fes")
    stop(sprintf("not a valid perf block. Found '%s' instead of 'fes'", header[1]))
  return(header[-1])
}

parseChunk <- function(chunk, header, d) {
  k <- length(header)
  if (length(chunk) != 5)
    stop("not a valid chunk.")

  chunk <- gsub("e \\+", "e+", chunk)
  
  n <- as.integer(unlist(strsplit(chunk[3], " "))[1])
  res <- data.frame(fun=header, d=d, n=n)
  for (i in 1:5) {
    fields <- unlist(strsplit(chunk[i], " "))
    l <- length(fields)
    nc <- l - k 
    dc <- (l-k+1):l
    what <- tolower(fields[nc])
    if (what == "med")
      what <- "median"
    res[[what]] <- as.numeric(fields[dc])
  }
  return(res)
}

readPerf <- function(filename, algo) {
  f <- file(filename, open="r")
  on.exit(close(f))
  res <- NULL  
  repeat {
    block <- readLines(f, n=17L)
    if (length(block) < 17L)
      break
    header <- parseHeader(block[2])
    d <- as.integer(strsplit(block[1], " ")[[1]][2])
    data <- cbind(algo,
                  rbind(parseChunk(block[3:7], header, d),
                        parseChunk(block[8:12], header, d),               
                        parseChunk(block[13:17], header, d)))    
    res <- rbind(res, data)
  }
  return(res)
}

cec2007_hv <- rbind(readPerf("mo_de-hv.txt", "mo_de"),
                    readPerf("mo_pso-hv.txt", "mo_pso"),
                    readPerf("nsga2_sbx-hv.txt", "nsga2_sbx"),
                    readPerf("nsga2_pcx-hv.txt", "nsga2_pcx"),
                    readPerf("mts-hv.txt", "mts"),
                    readPerf("mosade-hv.txt", "mosade"),
                    readPerf("gde3-hv.txt", "gde3"),
                    readPerf("demowsa-hv.txt", "demowsa")
                    )

cec2007_hv$metric <- "hv"

cec2007_r2 <- rbind(readPerf("mo_de-r2.txt", "mo_de"),
		    readPerf("mo_pso-r2.txt", "mo_pso"),
		    readPerf("nsga2_sbx-r2.txt", "nsga2_sbx"),
		    readPerf("nsga2_pcx-r2.txt", "nsga2_pcx"),
		    readPerf("mts-r2.txt", "mts"),
		    readPerf("mosade-r2.txt", "mosade"),
                    readPerf("gde3-r2.txt", "gde3"),
		    readPerf("demowsa-r2.txt", "demowsa")
                    )

cec2007_r2$metric <- "r2"

cec2007 <- rbind(cec2007_hv, cec2007_r2)

pdef <- unique(cec2007[, c("fun", "d", "n", "metric")])
pdef$pdef <- 1:nrow(pdef)

cec2007 <- merge(cec2007, pdef)
cec2007$metric <- as.factor(cec2007$metric)

save(cec2007, file="../../pkg/data/cec2007.rda")
