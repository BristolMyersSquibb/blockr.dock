% Please edit documentation in R/options-sidebar.R
\name{option_summary}
\alias{option_summary}
\alias{option_summary.default}
\alias{option_summary.dark_mode_option}
\alias{option_summary.compact_option}
\title{One-line summary of a board option's value}
\usage{
option_summary(x, value, ...)

\method{option_summary}{default}(x, value, ...)

\method{option_summary}{dark_mode_option}(x, value, ...)

\method{option_summary}{compact_option}(x, value, ...)
}
\arguments{
\item{x}{A board option (see \code{\link[blockr.core:new_board_options]{blockr.core::new_board_option()}}).}

\item{value}{The option's current value.}

\item{...}{Passed on to methods.}
}
\value{
A string, or \code{NULL} for nothing to show.
}
\description{
The board options sidebar lists each option category with a short summary
of its current values. By default the summary is built from the value:
text and single values as they are, \code{TRUE} as the option's name, \code{FALSE}
and empty values left out, a list of single values as those values, any
other list or longer vector by its size. A package
can give its option class a better line with a method, for instance to add
a unit ("8 turns") or a noun ("12 variables").
}
