\name{Obinary}
\alias{Obinary}
\alias{define_repops}
\alias{offartmb}
\title{Overloading glue between offarray and other packages such as RTMB}
\description{
Package \pkg{offartmb} lets you use \code{offarray} code directly in package \pkg{RTMB}. All you have to do is:

\itemize{
\item make sure you have done \code{library( offartmb)} \emph{after} \code{library( offarray)}
\item make sure the body of your objective function, and any hand-written sub-functions that is calls, is wrapped in a call to \code{\link{reclasso}} which is in package \pkg{offarray}. As its documentation says, there's no downside to doing that.
\item make sure you use \code{REPORTO( thing_I_want_to_keep)} to preserve interesting variables computed inside your function (like REPORT() in TMB or indeed package \pkg{RTMB}).
}

Then your function \code{myfun} will still run fine outside \code{RTMB}, but you should also be able to run \code{myfun_rtmb <- RTMB::MakeADFun( myfun, <paramvals>)} and everything will be copacetic thereafter.
}
\details{
You probably do not want to be reading this.

But, well, here you still are, so here it is. As you know, S3 classes (such as \code{offarray}) don't always play nicely with S4 classes (such as \code{advector} in package \pkg{RTMB}); the latter can be big bullies in terms of insisting that \emph{Their} class comes First, leading to downstream woe. So if you want S3 code to run both on "normal" R{} objects and on objects that might be S4, there is work to do--- either by you, or ideally behind-the-scenes automatically, which is where package \pkg{offartmb} can help. Particular problems occur with "double dispatch" on operators such as addition, where R{}'s built-in S3 dispatch rules are well-known to be borked. One option is to S4-ify the S3 class, and deal specifically with multiple inheritance, but that's a lot of work. Another option is to use something like \code{offarray::reclasso} (qv) to modify the code that is being run, to replace the base-R calls to eg "+" with calls to functions that know how to dispatch properly.

\code{reclasso} is an S3 generic dispatching on its \code{by} argument, and the default version actually makes no modifications. But there is a method \code{offartmb:::reclasso_advector} which (should) work on \code{advector}-class objects from package \pkg{RTMB}, ie when \code{RTMB::MakeADFun} (qv) is running your code. \code{reclasso_advector} makes some additional tweaks as well, eg to \code{REPORTO} for stashing results. If you wanted \code{offarray} to work with some other non-'RTMB' S4 package, you would need to write a similar generic. The additional tweaks are likely package-specific, but for operator-replacement the versions in \code{offartmb} itself might be usable as-is; read on.

It is pretty unlikely that you will \emph{ever} need to tinker with any of this yourself, but I need to document at least one function in order for this package to install smoothly! Anyway, even with \code{RTMB}, you \emph{might} conceivably need to add your own replacement operator/function for some weird thing that doesn't work out-of-the-box with \code{offarray} (although a lot of functions in \code{RTMB} actually seem to work OK). You can do so in two steps with \code{define_repops}, eg via

\preformatted{
  .besselZfun <- function( e1, e2, ...) Obinary( 'besselZ', e1, e2, ...)
  define_repops( besselZ=quote( .besselZfun))
}

The basic trick for most operators is to remove the \code{offarray} class from the operand(s), then call the base-R operator which will normally lead to some S4 method being invoked, then add back the \code{offarray} class and its dimensions etc to the result. Since many operators/functions follow a similar pattern, the function \code{Obinary} can be used to easily generate replacement operators. For example, the replacement for "*") is in effect

\preformatted{
  offartmb:::.Otimes <- function( e1, e2) Obinary( "*", e1, e2, FALSE)
}

\code{Obinary} is not actually specific to RTMB stuff, and might be useful in the event anyone ever needs to add similar functionality between \code{offarray} and some S4 package that is not \code{RTMB}.
}
\usage{
define_repops( ...)
# You would never just Obinary on its own like this...
Obinary(op, e1, e2, ..., allow_unary = FALSE)
}
\arguments{
\item{ ...}{in \code{define_repops}, a named list of replacement operator/functions and expressions to replace them with. The expressions normally need to be wrapped in \code{quote}--- you don't want \code{define_repops} itself to evaluate them. See \code{offartmb:::.onLoad} for example. If empty, the current repops will be returned. In \code{Obinary}, \code{...} can contain additional arguments for \code{op}, which will be passed to it unchanged. This is not meaningful for pure operators such as "*", but conceivably useful for eg statistical distributions where the mean, or mean-and-variance, might be S4; often the user might want to pass an extra argument such as "log=FALSE" or "df=5".}
\item{ op}{Name of an "operator". Usually eg "+" or similar but potentially any function which should dispatch based on its first two arguments.}
\item{ e1, e2}{Arguments whose class to dispatch on}
\item{ allow_unary}{A few functions, such as "-", have a legitimate unary variant; \code{-x} makes sense on its own, whereas \code{*x} doesn't.}
}
\value{\code{Obinary} returns a new function. \code{define.repops} normally returns (invisibly) the original set of replacement operator/functions.
}
\seealso{\code{offarray::reclasso}, \code{mvbutils::REPORTO}, \code{RTMB::MakeADFun}
}
\examples{
## should have one, I guess...
}
\keyword{misc}
