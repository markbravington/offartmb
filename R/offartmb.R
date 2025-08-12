# This is package offartmb 

".ADREPORTO" <-
function(...){
  dots <- match.call( expand.dots=FALSE)$...

  # Allow auto-creation, eg sqrt_Prob_x= sqrt( Prob_x)  
  namdots <- names( dots) %||% rep( '', length( dots))
stopifnot( all( nzchar( namdots) | sapply( dots, is.name)))
  
  for( ivar in seq_along( dots)){
    if( nzchar( namdots[ ivar])){ # create this var
      eval.parent( call( 'assign', namdots[ ivar], dots[[ ivar]]))
    } else {
      namdots[ ivar] <- as.character( dots[[ ivar]])
    }
    
    eval.parent( call( 'ADREPORT', as.name( namdots[ ivar])))
  }
return( NULL)
}


".eval" <-
function( expr, ...){
  mc <- match.call( expand.dots=TRUE)

  # Inside 'reclasso', don't evaluate and don't call substititute( expr):
  mc$expr <- reclasso.advector( expr, .sub=FALSE)
  mc[[1]] <- quote( eval)
eval.parent( mc)
}


".eval.parent" <-
function( expr, ...){
  mc <- match.call( expand.dots=TRUE)
  
  # Inside 'reclasso', don't evaluate and don't call substititute( expr):
  mc$expr <- reclasso.advector( expr, .sub=FALSE)  
  mc[[1]] <- quote( eval.parent)
eval.parent( mc)
}


".local.return" <-
function(...){
r"--{
This is a copy of 'debug:::debug.local.return', to get around a problem with 'reclasso' inside 'mvbutils::mlocal', ie 

locfun <- function( nlocal=sys.parent()) mlocal( reclasso( by=<...>, { 
  ...; 
  return( local.return( ...)) # OK without RTMB, but not with it
}))

which doesn't work; it tries to assign 'override.answer' into 'baseenv()', which is clearly not what's supposed to happen! So 'reclasso.advector' now substitutes this function in place of calls to 'local.return'.

I suspect the whole return-value stuff in 'mlocal' needs an overhaul (it's over 20 years old...) but this will do for now!

Almost the same as local.return(), but searches in a different way for where to put 'override.answer', cos the eval( enclos) trick in local.return doesn't work inside debugger in R4.1 (not sure when that problem started). That trick relies on parent.frame(2) working OK, and might be delicate anyway;  this code should be more robust, in general, even outside debug.
}--"

  orig.mc <- mc <- as.list( match.call())[ -1]

  if( length( mc)) {
    if( length( mc)==1)
      mc <- eval( mc[[1]], envir=parent.frame())
    else { # multiple arguments, so return as named list
      if( is.null( names( mc)))
        which <- 1:length( mc)
      else
        which <- names( mc)==''

      for( i in index( which))
        if( is.symbol( orig.mc[[ i]]))
          names( mc)[ i] <- as.character( orig.mc[[ i]] )
      mc <- lapply( mc, eval, envir=parent.frame())
    }
  }

  # Find the mlocal() frame that called me
  # enclos <- parent.frame( 2)$enclos # non-mtraced version; the problem is, what should 2 be when debugging?
  # Instead, look for it...
  lastpf <- .GlobalEnv
  pfgen <- 1
  repeat{
    pf <- parent.frame( pfgen)
    if( identical( pf, lastpf)){
stop( "Could not find my mlocal caller :(")
    }
    if( exists( '_ENCLOS_', pf, mode='environment', inherits=FALSE)){
      # scatn( 'mlocal found at gen %i', pfgen)
      enclos <- pf$'_ENCLOS_'
      # print( head( lsall( enclos)))
  break
    }
    pfgen <- pfgen + 1
    lastpf <- pf
  }

  
  assign( 'override.answer', mc, envir=enclos)
}


".Oatan2" <-
function( e1, e2) Obinary( 'atan2', e1, e2, allow_unary=FALSE)


".Odiv" <-
function( e1, e2) Obinary( '/', e1, e2, allow_unary=FALSE)


".Ominus" <-
function( e1, ...) Obinary( '-', e1, ..., allow_unary=TRUE)


".onLoad" <-
function( libname, pkgname){
  # Ensure 'advector' works properly during subassignment to 'offarray' objects
  evalq( envir=offarray:::special_stuff, 
    casting_classes <- c( casting_classes,
        list( advector= quote( RTMB::advector))
  ))
  
  # Operator "overloads". User can add in-session via 'define_repop'
  ns <- asNamespace( pkgname)
  e <- new.env( parent=baseenv())
  ns$overloads <- e
  e$repops <- list( 
    '+'= quote( offartmb:::.Oplus),
    '*'= quote( offartmb:::.Otimes),
    '-'= quote( offartmb:::.Ominus),
    '/'= quote( offartmb:::.Odiv),
    '^'= quote( offartmb:::.Opow),
    'atan2'= quote( offartmb:::.Oatan2),
    'REPORTO'= quote( offartmb:::.REPORTO),
    'ADREPORTO'= quote( offartmb:::.ADREPORTO),
    eval= quote( offartmb:::.eval),
    eval.parent= quote( offartmb:::.eval.parent),
    local.return= quote( offartmb:::.local.return)
  )
  
  # Could also call eg:
  # define_repops( '+'=quote( offartmb:::.Oplus), <etc>)
}


".Oplus" <-
function( e1, ...) Obinary( '+', e1, ..., allow_unary=TRUE)


".Opow" <-
function( e1, e2) Obinary( '^', e1, e2, allow_unary=FALSE)


".Otimes" <-
function( e1, e2) Obinary( '*', e1, e2, allow_unary=FALSE)


".REPORTO" <-
function( ..., names=NULL){
  dots <- match.call( expand.dots=FALSE)$...
stopifnot( all( sapply( dots, is.name)))
  names <- c( as.character( dots), names)
  
  for( v in names) eval.parent( call( 'REPORT', as.name( v)))
return( NULL)
}


"ADREPORTO" <-
function( ...){
  # Does nothing; substituted with "real deal" by reclasso.advector() etc
}


"define_repops" <-
function( ...){
  if( !...length()){
return( overloads$repops)
  }
  
  l <- list( ...)
stopifnot( all( nzchar( names( l))))

  oldio <- overloads$repops
  overloads$repops[ names(l) ] <- l
  overloads$repops <- overloads$repops %SUCH.THAT% !is.null( .)  
invisible( oldio)
}


"Obinary" <-
function( op, e1, e2, ..., allow_unary=FALSE){
## Rather than trying to understand S4, I am implementing my own double-dispatch.
## If yer want it done properly, do it yerself
  
  if( missing( e2)){
    if( allow_unary){
return( get( op)( e1))
    } else {
stop( sprintf( "Need two operands for '%s'", op))
    }
  } # if unary/missing
  
  o1 <- inherits( e1, 'offarray')
  o2 <- inherits( e2, 'offarray')
  if( o1 && o2){
stopifnot( identical( unname( dimseq( e1)), unname( dimseq( e2))),
    identical( unname( attr( e1, 'offset')), unname( attr( e2, 'offset'))))
  }
  
  # Deconfuse R's multiple-dispatch "system"
  # unclass() is expensive (deep copy) but attribute-setting is not
  if( o1){
    oldClass( e1) <- oldClass( e1) %except% 'offarray'
  }
  if( o2){
    oldClass( e2) <- oldClass( e2) %except% 'offarray'
  }
  
  res <- get( op)( e1, e2, ...)
  if( o1 || o2){
return( offarray( res, dimseq= dimseq( if( o1) e1 else e2)))
  } else {
return( res)
  }
}


"reclasso.advector" <-
function( expr, by, evalfr=parent.frame(), 
    DNR=character(), .sub=TRUE, ...
){
## Replace calls in expr to +,-,*,/,atan2, and any user-defined additions with 
## calls to offarray-compatible equivs (that will still honour advector)
## see .onLoad for default list
## evalfr in case this gets invoked indirectly, by reclasso.list

  r"--{
  Want this:
  
  expr <- do.call( 'substitute', list( expr, overloads$repops))
  This used to work (at some point with R4.4, and various package versions), and still should but doesn't. It's R bugs, probably due to the bloody byte compiler again. It usually leads to errors like this:
  
  Error in `[<-.default`(x, ..., value = value) : subscript out of bounds
  
  Or sometimes reclasso wouldn't work at all (ie would not replace operators), leading to warnings about "Incompatible methods" which are terminal in this context.

  Hence, do it manually:
  }--"

  subcall <- quote( substitute( x, y))
  subcall[[2]] <- if( .sub) substitute( expr) else expr
  subcall[[3]] <- overloads$repops %without.name% DNR
  expr <- eval( subcall) 

  # or if I had the user-tweakable version in place


  # NOW the problem is that '[<-' doesn't work properly, coz it has been hijacked by RTMB
  # and it passes the LHS thru 'advector()', which sets the class of an offarray to 'c("advector","offarray")'
  # whereas I want it the other way round.

  # Check subassigment shenanigans:
  ff <- as.environment( evalq( find( '[<-'), parent.frame()))
  if( !identical( ff, baseenv())){
    oldsuba <- ff$'[<-'

    #  Check if done already...
    if( is.null( attr( body( oldsuba), 'I_have_been_replaced'))){
      # This is possibly undesirable, coz it _replaces_ the already-replaced '[<-'
      # Alternative would be to hack a new environment descended from parent.frame(), 
      # containing this new definition,
      # and evaluate expr there, rather than in parent.frame()

      # enew <- new.env( parent=environment( oldsuba))
      newsuba <- oldsuba
      newbod <- quote({ 
        if (inherits(value, "advector")) {
          if (is.numeric(x)) 
            x <- advector(x)
          ioff <- match( 'offarray', oldClass( x), 0);
          if( ioff){
            oldClass( x) <- c( 'offarray', oldClass( x)[ -ioff])
          } # so that '[<-.offarray' gets called next (it will subsequently call '[<-.advector')
        }
        ret <- base::"[<-"(x, ..., value = value)      
        ioff <- match( 'offarray', oldClass( ret), 0);
        if( ioff){
          oldClass( ret) <- c( 'offarray', oldClass( ret)[ -ioff])
        }
        ret
      })
    
      attr( newbod, 'I_have_been_replaced') <- TRUE
      body( newsuba) <- newbod
      assign( '[<-', newsuba, ff)
    } # if subass not re-replaced yet
  } # if need to re-replace subassignment

  if( .sub){
return( eval( expr, evalfr))
  } else {
return( expr)
  }
}


"sumover.advector" <-
function( x, mm, drop=FALSE) {
## This avoids trying to condense dims & then call .colSums/.rowSums,
## which only apply to numerics
## so it might be a bit slower, but at least it might work
## NB colSums.advector uses apply
 
  if( is.character( mm)){
    mm <- match( mm, c( names( dim( x)), names( dimnames( x))), 0) # one will work...
  }
  ds <- dimseq( x)
  
  y <- apply( as.array( x, make_dimnames=FALSE), seq_along( dim( x))[-mm], sum)
  
  # ?drop-stuff goes here?

return( offarray( y, dimseq=ds[ -mm]))
}

