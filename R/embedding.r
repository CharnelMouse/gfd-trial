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

prune_embedding <- function(x, rules, progress = FALSE) {
  if (length(x$N) == 0)
    return(x)

  # embind converts a pattern back to an E index
  nonfixed <- which(is.na(x$V[1, ]))
  embind <- function(x) {
    y <- x[nonfixed]
    as.integer(1L + sum((1L*(!y) + 2L*y)*3L^(seq_along(y) - 1L), na.rm = TRUE))
  }
  embinds <- apply(x$V, 1, embind)
  embinds_corr <- embinds == seq_along(x$N)
  if (!all(embinds_corr)) {
    stop(print(which(!embinds_corr)), print(x$V[!embinds_corr, ]), print(embinds[!embinds_corr]))
  }

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

  # merge rules with same detset
  detset_list <- apply(detset_mat, 1, identity, simplify = FALSE)
  depset_list <- apply(depset_mat, 1, identity, simplify = FALSE)
  detset_match <- match(detset_list, detset_list)
  detset_uniq <- unique(detset_match)
  detset_mat <- detset_mat[detset_uniq, , drop = FALSE]
  depset_mat <- do.call(
    rbind,
    tapply(
      depset_list,
      detset_match,
      \(x) Reduce(\(x, y) ifelse(is.na(x), y, x), x),
      simplify = FALSE
    )
  )
  if (progress) {
    cat(paste0(nrow(detset_mat), "/", length(rules), " unique rule detsets", "\n"))
    flush.console()
  }

  rem <- rep(FALSE, length(x$N))
  attrs <- colnames(x$V)

  # only apply first matching rule to each embedding that has an effect,
  # to get a "parent".
  bool_list <- apply(x$V, 1, identity, simplify = FALSE)
  # very slow for large number of embeddings
  bool_match <- vapply(
    seq_along(x$N),
    \(n) {
      if (progress) {
        cat(paste0("\r", n, "/", length(x$N)))
        flush.console()
      }
      x <- bool_list[[n]]
      dep_unsatis <- which(!apply(
        depset_mat,
        1,
        satisfied,
        by = x
      ))
      if (length(dep_unsatis) == 0)
        return(n)
      det_satis <- apply(
        detset_mat[dep_unsatis, , drop = FALSE],
        1,
        satisfied,
        by = x
      )
      if (!any(det_satis))
        return(n)
      comb <- rbind(
        x,
        depset_mat[dep_unsatis[det_satis], , drop = FALSE]
      )
      comb <- apply(
        comb,
        2,
        \(x) unique(na.omit(x)),
        simplify = FALSE
      )
      if (any(lengths(comb) == 2))
        return(NA_integer_)
      comb <- vapply(
        comb,
        \(x) c(x, NA)[[1]],
        logical(1)
      )
      embind(comb)
    },
    integer(1)
  )
  # can then follow parents to find root as replacement,
  # to avoid repeated re-executing of the rules
  bool_match <- fixed_point(bool_match, \(x) bool_match[x], identical)
  for (n in seq_along(x$N)) {
    if (progress) {
      cat(paste0("\r", n, "/", length(x$N)))
      flush.console()
    }
    bools <- bool_list[[n]]
    bm <- bool_match[[n]]
    new_bools <- bool_list[[bm]]
    if (is.na(bm)) {
      rem[[n]] <- TRUE
      next
    }
    if (bm != n) {
      replacement <- bm
      rem[[n]] <- TRUE
      x$E[x$E == n] <- replacement
    }
  }
  if (progress) {
    cat("\n")
    flush.console()
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

satisfied <- function(x, by) {
  all(is.na(x) | (!is.na(by) & x == by))
}

fixed_point <- function(init, step, endif) {
  old <- init
  current <- step(init)
  while (!endif(old, current)) {
    old <- current
    current <- step(current)
  }
  current
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
