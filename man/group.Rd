% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/group.R
\name{group}
\alias{group}
\title{group a set of nodes and flows}
\usage{
group(
  f,
  args = list(),
  oma = rep(0.1, 4),
  group_col = NULL,
  border_col = "black",
  label = "",
  label_pos_x = 0.02,
  label_pos_y = 0.9,
  label_font = 1,
  label_col = "black",
  label_cex = 1,
  label_x = NULL,
  label_y = NULL,
  ...
)
}
\arguments{
\item{f}{flodia plot function}

\item{args}{list of named arguments to \code{f}}

\item{oma}{required border around nodes, vector of length 4 specifying:
bottom, left, top, right. defaults to 0.1 on all sides}

\item{group_col}{colour of group box}

\item{border_col}{colour of group border}

\item{label}{label to draw on group}

\item{label_pos_x}{a decimal between 0 and 1 giving the position on the group
to draw \code{label}, where 0 = left and 1 = right}

\item{label_pos_y}{a decimal between 0 and 1 giving the position on the group
to draw \code{label}, where 0 = bottom and 1 = top}

\item{label_font}{font of \code{label}, defaults to 1}

\item{label_col}{colour of \code{label}, defaults to black}

\item{label_cex}{label cex, defaults to 1}

\item{label_x}{x co-ordinate of label position, overrides \code{label_pos_x}}

\item{label_y}{y co-ordinate of label position, overrides \code{label_pos_y}}

\item{...}{further arguments to \code{rect()} for formatting group}
}
\value{
coordinates of group
}
\description{
group a set of nodes and flows
}
