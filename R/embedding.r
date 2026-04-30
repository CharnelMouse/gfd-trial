embedding <- function(V, E, A, N) {
  structure(
    list(V = V, E = E, A = A, N = N),
    class = "embedding"
  )
}

gv_embedding <- function(x, name = NA_character_, ...) {
  dotV <- paste(
    seq_len(nrow(x$V)),
    " [label = \"",
    x$N,
    "\"];",
    sep = "",
    collapse = "\n"
  )
  dotE <- paste(
    x$E[, "parent"],
    " -> ",
    x$E[, "child"],
    ";",
    sep = "",
    collapse = "\n",
    recycle0 = TRUE
  )
  paste("digraph {", dotV, dotE, "}", sep = "\n")
}

prune_embedding <- function(x, rules, ...) {
  # convert rule FDs assuming negated presence columns are all placed afterwards
  ao <- attrs_order(rules)
  stopifnot(length(ao) %% 2 == 0)
  len <- length(ao) %/% 2L
  rule_detsets <- lapply(
    detset(rules),
    \(x) ao[((match(x, ao) - 1L) %% len) + 1L]
  )
  presence_detsets <- lapply(
    detset(rules),
    \(x) match(x, ao) <= len
  )
  rule_deps <- dependant(rules)
  presence_depsets <- match(rule_deps, ao) <= len
  rule_deps <- ao[((match(rule_deps, ao) - 1L) %% len) + 1L]
  true_ao <- ao[seq_len(len)]
  detset_mat <- Map(
    \(r, p) {
      res <- rep(NA, len)
      res[match(r, ao)] <- p
      res
    },
    rule_detsets,
    presence_detsets
  ) |>
    do.call(what = rbind) |>
    (`colnames<-`)(true_ao)
  depset_mat <- Map(
    \(r, p) {
      res <- rep(NA, len)
      res[match(r, ao)] <- p
      res
    },
    rule_deps,
    presence_depsets
  ) |>
    do.call(what = rbind) |>
    (`colnames<-`)(true_ao)

  rem <- rep(FALSE, length(x$N))
  attrs <- colnames(x$V)
  for (n in seq_along(x$N)) {
    bools <- x$V[n, ]
    new_bools <- bools
    for (r in seq_along(rules)) {
      detpres <- detset_mat[r, ]
      deppres <- depset_mat[r, ]
      if (all(is.na(detpres) | (!is.na(new_bools) & detpres == new_bools))) {
        if (all(is.na(deppres) | is.na(new_bools) | deppres == new_bools))
          new_bools <- ifelse(is.na(deppres), new_bools, deppres)
        else {
          # remove
          new_bools <- NULL
          break
        }
      }
    }
    if (is.null(new_bools)) {
      rem[[n]] <- TRUE
      next
    }
    if (!identical(new_bools, bools)) {
      replacement <- which(apply(x$V, 1, identical, new_bools))
      stopifnot(length(replacement) == 1)
      rem[[n]] <- TRUE
      x$E[x$E == n] <- replacement
    }
  }
  keep <- which(!rem)
  x$E[] <- match(x$E, keep)
  edge_keep <- apply(x$E, 1, Negate(anyNA))
  x$E <- x$E[edge_keep, , drop = FALSE]
  x$A <- x$A[edge_keep]
  edge_nonself <- with(x, E[, "child"] != E[, "parent"])
  x$E <- x$E[edge_nonself, , drop = FALSE]
  x$A <- x$A[edge_nonself]
  Elst <- if (nrow(x$E) == 0) {
    list()
  }else{
    apply(x$E, 1, identity, simplify = FALSE)
  }
  mats <- match(Elst, Elst)
  uniq <- unique(mats)
  x$A <- lapply(
    uniq,
    \(n) intersect(
      colnames(x$V),
      unique(do.call(c, x$A[mats == n]))
    )
  )
  x$E <- x$E[uniq, , drop = FALSE]
  x$V <- x$V[keep, , drop = FALSE]
  x$N <- x$N[keep]
  x
}

`[.embedding` <- function(x, i) {
  inds <- setNames(seq_along(x$N), x$N)
  i <- inds[i]
  rem <- inds[!is.element(inds, i)]
  x$E <- as.data.frame(x$E, check.names = FALSE)
  grps <- tapply(
    x$E,
    vapply(x$A, toString, character(1)),
    identity,
    simplify = FALSE
  )
  grps <- lapply(
    grps,
    Reduce,
    f = remove_embedding_from_fdks,
    x = rem
  )
  x$E <- as.matrix(Reduce(
    rbind,
    grps,
    init = x$E[FALSE, , drop = FALSE]
  ))
  x$E[] <- match(x$E, i)
  stopifnot(!anyNA(x$E))
  x$A <- rep(
    names(grps),
    vapply(grps, nrow, integer(1))
  ) |>
    strsplit(", ")
  x$N <- x$N[i]
  x$V <- x$V[i, , drop = FALSE]
  x
}

`[[.embedding` <- function(x, i) {
  inds <- setNames(seq_along(x$N), x$N)
  i <- try(inds[[i]], silent = TRUE)
  if (class(i)[[1]] == "try-error")
    stop(attr(i, "condition")$message)
  x[i]
}

remove_embedding_from_fdks <- function(g, n) {
  rbind(
    subset(g, child != n & parent != n),
    expand.grid(
      child = g$child[g$parent == n],
      parent = g$parent[g$child == n]
    )
  )
}

parents <- function(x, children) {
  unique(x$E[, "parent"][x$E[, "child"] %in% children])
}

children <- function(x, parents) {
  unique(x$E[, "child"][x$E[, "parent"] %in% parents])
}
