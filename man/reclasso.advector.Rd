\name{reclasso.advector}
\alias{reclasso.advector}
\title{Prepare sensitive code for RTMB}
\description{
\code{reclasso} is documented in package \pkg{offarray}. It is meant for \code{offarray}-type code, but offers a simple way to make S3 classes work alright with \code{RTMB} S4 class \code{advector}, to work around fairly-well-known problems with R{}'s dispatch mechanism for binary operators like \code{+}. Go read its documentation, if you haven't already! And then you probably don't need to read the rest of this.

\code{reclasso.advector} is the method for \code{advector} arguments from \code{RTMB}. You'll never need to call \code{reclasso.advector} directly, and you don't normally need to know about its details--- except that \emph{occasionally} the \code{DNR} argument might be useful, if you \emph{don't} want some calls to be overwritten with RTMB-friendly replacements (see \code{\link{local.return}} below). \code{DNR} is the only reason I'm documenting \code{reclasso.advector}.

In addition to binary operators, \code{reclasso.advector} will by default also work recursively thru the arguments of calls to \code{\link{eval}} and \code{eval.parent}. I think that's probably the most useful behaviour, because you can then embed "nested" code in your function that "lives" outside it. (You can also do that via \code{mvbutils::mlocal} functions, which I tend to prefer---but that is one more non-base-R thing for other people to have to learn about.). Anyway, if you really don't want \code{reclasso} to do anything when \code{eval()} is called, then you could set \code{DNR=c"eval","eval.parent")} as an argument when you call \code{reclasso}.

And it now (v1.0.35+) also replaces calls to \code{local.return} (which is sometimes used inside \code{mvbutils::mlocal} functions) to get around some nasty details. This is one case where, if you don't use \code{mlocal}, you \emph{might} just conceivably have an object of your own called \code{local.return} for some completely different purpose--- in which case, \code{DNR="local.return"}.

Note that there's no need for special treatment of \code{evalq}, \code{local}, \code{with}, etc, because their expression-arguments are directly there in the code, so will get the benefit of \code{substitute}.
\subsection{Debugging}{Currently (v1.0.35), the \pkg{debug} package isn't quite getting it right when \code{reclasso.advector} is running (i.e. only within \code{MakeADFun} calls)--- though things work fine away from \code{MakeADFun}. The \code{repops} substitution isn't happening. It will be fixable, but I don't have time right now.
}
}
\details{\code{reclasso.advector} calls \code{substitute} to do the hard work. If you really want, or if you are really silly, you can defeat the mechanisms of \code{reclasso.advector} by using devious things like \code{do.call("name_of_function")}--- entirely at your own risk.
}
\usage{
\method{reclasso}{advector}( expr, by, evalfr = parent.frame(),
    DNR = character(), .sub=TRUE, ...) # S3 method for advector
}
\arguments{
\item{ expr, by, evalfr, ...}{see \code{\link{reclasso}}}
\item{ DNR}{Character vector of function-names to \emph{not} replace with S3+S4-friendly replacements.}
\item{ .sub}{Leave this alone, o user! It determines whether \code{expr} is passed thru \code{substitute} and later thru \code{eval}. \code{.sub=FALSE} is only needed when \code{reclasso.advector} is called recursively, e.g. by \code{offartmb:::.eval}.}
}
\examples{
## Maybe I should have one here to demo 'DNR', but it's tedious...
## and 'DNR' is very unlikely to be needed anyway
}
\keyword{internal
}
