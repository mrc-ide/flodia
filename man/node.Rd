% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/node.R
\name{node}
\alias{node}
\title{add a node}
\usage{
node(
  x,
  y,
  r = 0.1,
  label = "",
  rx = r,
  ry = r,
  node_col = "grey80",
  border_col = "black",
  label_font = 1,
  label_col = "black",
  label_cex = 1,
  ...
)
}
\arguments{
\item{x}{cental x position}

\item{y}{central y position}

\item{r}{radius of node}

\item{label}{node label}

\item{rx}{radius of node in x-direction}

\item{ry}{radius of node in y-direction}

\item{node_col}{colour of node}

\item{border_col}{colour of node border}

\item{label_font}{label font, see \code{par()}}

\item{label_col}{colour of \code{label}}

\item{label_cex}{size of \code{label} text}

\item{...}{other formatting arguments to \code{rect()}}
}
\description{
add a node
}
