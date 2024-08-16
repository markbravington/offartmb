# This is package offartmb 

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
    'REPORTO'= quote( offartmb:::.REPORTO)
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
function( expr, by, evalfr=parent.frame()){
## evalfr in case this gets invoked indirectly, by reclasso.list
  
# scatn( 'reclasso for advector')
  # Fix +,-,*,/,atan2, and any user-defined additions:
  # see .onLoad for default list
  expr <- do.call( 'substitute', list( expr, overloads$repops))

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
  
  eval( expr, evalfr)
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

