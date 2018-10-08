resample <- function(x, ...) {
  x[sample.int(length(x), ...)]
  }

# Remember: GaleShapeley favors the proposers (proposers are men) 
# You can let other parties become man/woman. 

# unmen, unwom: unmatched men and women
# menmatch1: same ncols as manpref=, each column is a man. value is the assigned woman.
# wommatch1: same ncols as wompref=, each column is a woman. value is the assigned man.
# menronw, womronm: man's rank on the assigned woman's choices, and vice versa.
# mp: "residuals" of the manpref=, 0 indicates cells where the algorithm processed.

mysm <- function(manpref, wompref) {
  mporig <- manpref
  smen <- 1:ncol(manpref)  # set of single men yet to be matched-up
  smen <- smen[ apply(manpref, 2, function(x) sum(!is.na(x)) >= 1 ) ]
  stepinto <- TRUE
  menmatch <- matrix(ncol=ncol(manpref))  # store the matches, to be output
  quota <- rep(1, ncol(wompref))
  wommatch <- matrix(ncol=ncol(wompref))
  while (stepinto) {
    smen <- smen[sapply(smen, function(x) sum(manpref[, x], na.rm=TRUE) > 0)]
    # id <- resample(1:length(smen), 1)  # alternatively, set id <- 1
    id <- 1
    proposer <- smen[id]
    smen <- smen[-id]
    rowidx <- min(which(manpref[, proposer] > 0))  # returns Inf when proposer is NA, or when all depleted
    if ( is.infinite(rowidx) ) {break}
    selected <- manpref[rowidx, proposer]  # selected woman
    manpref[rowidx, proposer] <- 0  # remove selected woman
    ( manrank <- which(wompref[, selected] == proposer) ) # man's rank on women's preference
    ( inmen <- na.omit(wommatch[1, selected]) )  # incumbent men
    if ( length(inmen) == 0 ) {inmenrank <- -Inf} else {
      inmenranks <- match(inmen, wompref[, selected])  # multiple ranks
      inmenranks[is.na(inmenranks)] <- Inf  # not on women's preference, hence hated
      ( inmenrank <- max(inmenranks) )  # single rank, the largest
    }
    # match(c(7, 2, 9), wompref[, selected]); match returns position ids
    
    if ( length(manrank)==0 ) {manrank <- Inf}  # proposer man is hated by woman
    if ( length(inmenrank)==0 | is.na(inmenrank) ) {inmenrank <- Inf}  # incumbent man is hated
    # when man is not on women's preference list, it becomes an argument of length zero (which fails elseif condition)
    if ( any(is.na(wommatch[1, selected])) ) {
      menmatch[1, proposer] <- selected
      wommatch[1, selected] <- proposer
    } else if ( manrank < inmenrank ) {
      ko <- which.max(inmenranks)
      kickout <- wommatch[1, selected]
      menmatch[1, proposer] <- selected
      menmatch[1, kickout] <- NA
      wommatch[1, selected] <- proposer
      smen <- c(smen, kickout)
    } else {
      smen <- c(smen, proposer)
    }
    if ( length(smen)==0 ) {stepinto <- FALSE}
    # if ( length(smen)==0 | is.infinite(rowidx) )
    # rowidx is infinite when all menpref are depleted
  }  # while
  nawom <- colnames(wompref)[is.na(wommatch)]
  namen <- colnames(manpref)[is.na(menmatch)]
  outw <- matrix(ncol=ncol(wompref))  # man's rank on women's preferences
  for (o in 1:length(wommatch)) {
    findmen <- na.omit(wommatch[1, o])  # man's rank on woman's preferences
    if (length(findmen)!=0) {
      manrank <- which(wompref[, o] == findmen)
      if (length(manrank)==0) {outw[1, o] <- Inf} else outw[1, o] <- manrank
    } 
  }
  outm <- matrix(ncol=ncol(manpref)) # woman's rank on men's preferences
  for (o in 1:length(menmatch)) {
    findwom <- na.omit(menmatch[1, o])  
    if (length(findwom)!=0) {
      womrank <- which(mporig[, o] == findwom)
      if (length(womrank)==0) {outm[1, o] <- Inf} else outm[1, o] <- womrank
    } 
  }
  return(list(smen=smen, unmen=namen, unwom=nawom, menmatch1=menmatch, wommatch1=wommatch, menronw=outw, womronm=outm, mp=manpref))
}
