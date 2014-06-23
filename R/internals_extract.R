

################################################################################
### extraction funs

################################################################################
.resamplingMethod <- function(holdOutTable) {
  
  typ <- list(cv=FALSE, repeatedcv=FALSE, boot=FALSE, pu=FALSE)
  tstStrng <- holdOutTable$Resample[1]
  
  ### check if cv, repeatedcv, or boot
  if ( length(grep('Fold', tstStrng))==1 & length(grep('Rep', tstStrng))!=1 )
    typ$cv <- TRUE
  if ( length(grep('Rep', tstStrng))==1 )
    typ$repeatedcv <- TRUE
  if ( length(grep('Resample', tstStrng))==1 )
    typ$boot <- TRUE
  
  ### check if Pu or not given cv or repeatedcv
  if ( typ$cv | typ$repeatedcv ) {
    if (typ$repeatedcv) {
      rp1 <- strsplit(holdOutTable$Resample[1], split='[.]')[[1]][[2]]
      holdOutTable <- holdOutTable[grepl(rp1, holdOutTable$Resample),]
    }
    ufolds <- unique(holdOutTable$Resample)
    idx.f1 <- holdOutTable$Resample==ufolds[1]
    idx.f2 <- holdOutTable$Resample==ufolds[2]
    intrsctn <- intersect(holdOutTable$rowIndex[idx.f1], holdOutTable$rowIndex[idx.f2])
    typ$pu <- length(intrsctn)!=0
  }
  if ( typ$boot ) {
    ### we assume that the unlabeled samples are the same
    urs <- unique(holdOutTable$Resample)
    
    idx.r1.un <- holdOutTable$Resample==urs[1] & holdOutTable$obs=='un'
    
    typ$pu <- all(sapply(1:length(urs), function(i) {
      idx.r.un <- holdOutTable$Resample==urs[i] & holdOutTable$obs=='un'
      identical(sort(holdOutTable$rowIndex[idx.r1.un]),
                sort(holdOutTable$rowIndex[idx.r.un])) } ))
  }
  
  return(typ)
}


.extractCv <- function(holdOutTable) {
  rtrn <- list()
  rtrn$pos <- holdOutTable$pos[holdOutTable$obs=='pos']
  names(rtrn$pos) <- holdOutTable$rowIndex[holdOutTable$obs=='pos']
  rtrn$un <- holdOutTable$pos[holdOutTable$obs=='un']
  names(rtrn$un) <- holdOutTable$rowIndex[holdOutTable$obs=='un']
  return(rtrn)
}
.extractRepeatedcv <- function(holdOutTable) {
  ### split folds form repetitions
  splt <- strsplit(holdOutTable$Resample, split='[.]')
  ### repetition group index
  rpt <- sapply(splt, function(x) x[2])
  urpt <- unique(rpt)
  ### list of held out samples of repetition
  rtrn <- lapply(urpt, function(x) .extractCv(holdOutTable[grepl(urpt[1], 
                                                                 holdOutTable$Resample), ]) )
  names(rtrn) <- urpt
  ### make a list for p and u
  rtrn <- list(pos = lapply(rtrn, function(x) x$pos), 
               un = lapply(rtrn, function(x) x$un) ) 
  return(rtrn)
}
.extractBoot <- .extractBootPu <- function(holdOutTable) {
  rtrn <- list()
  ursmpls <- unique(holdOutTable$Resample)
  rtrn$pos <- lapply(ursmpls, function(x) {
    idx <- holdOutTable$Resample==x & holdOutTable$obs=='pos'
    rtrn <- holdOutTable[idx , 'pos']
    names(rtrn) <- holdOutTable[idx, 'rowIndex']
    return(rtrn) })
  rtrn$un <- lapply(ursmpls, function(x) {
    idx <- holdOutTable$Resample==x & holdOutTable$obs=='un'
    rtrn <- holdOutTable[idx , 'pos']
    names(rtrn) <- holdOutTable[idx, 'rowIndex']
    return(rtrn) })
  names(rtrn$pos ) <- ursmpls
  names(rtrn$un ) <- ursmpls
  
  return(rtrn)
}
.extractCvPu <- function(holdOutTable) {
  rtrn <- list()
  rtrn$pos <- holdOutTable$pos[holdOutTable$obs=='pos']
  names(rtrn$pos) <- holdOutTable$rowIndex[holdOutTable$obs=='pos']
  
  nms <- unique(holdOutTable$Resample)
  
  rtrn$un <- lapply(nms, function(x) {
    idx <- holdOutTable$obs=='un' &  holdOutTable$Resample==x
    rtrn <- holdOutTable$pos[idx]
    names(rtrn) <- holdOutTable$rowIndex[idx]
    return(rtrn) }
  )
  names(rtrn) 
  return(rtrn)
}
.extractRepeatedcvPu <- function(holdOutTable) {
  ### split folds form repetitions
  splt <- strsplit(holdOutTable$Resample, split='[.]')
  ### repetition group index
  rpt <- sapply(splt, function(x) x[2])
  urpt <- unique(rpt)
  ### list of held out samples of repetition
  rtrn <- lapply(urpt, function(x) .extractCvPu(
    holdOutTable[grepl(urpt[1], holdOutTable$Resample), ]) )
  names(rtrn) <- urpt
  rtrn.pos <- lapply(rtrn, function(x) x$pos)
  rtrn.un <- lapply(rtrn, function(x) x$un)
  rtrn <- list(pos=rtrn.pos, un=rtrn.un)
  return(rtrn)
}