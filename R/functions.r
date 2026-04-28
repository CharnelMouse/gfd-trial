to_presence <- function(x) {
  x[] <- lapply(x, Negate(is.na))
  x
}

sort_fds <- function(x) {
  x[order(match(dependant(x), attrs_order(x)))]
}

group_fds <- function(x) {
  dets <- detset(x)
  inds <- match(dets, dets)
  res <- data.frame(a = unique(inds))[-1]
  res$det <- dets[unique(inds)]
  res$dep <- split(dependant(x), inds)
  res
}

find_rules <- function(x) {
  DATAFRAME(apriori(
    transactions(x),
    parameter = list(
      support = 1/nrow(x),
      confidence = 1
    )
  ))
}

fds_from_rules <- function(x, attrs_order) {
  lhs <- as.character(x$LHS)
  rhs <- as.character(x$RHS)
  functional_dependency(
    Map(
      list,
      substr(lhs, 2, nchar(lhs) - 1) |>
        strsplit(","), # remove braces
      substr(rhs, 2, nchar(rhs) - 1) # remove braces
    ),
    attrs_order
  )
}

minimise_rulefds <- function(x) {
  x[apply(outer(x, x, `>`), 1, Negate(any))]
}

embeddings <- function(x) {
  nms <- names(x)
  vals <- lapply(setNames(nm = nms), \(x) c(NA, FALSE, TRUE))
  embs <- as.matrix(expand.grid(vals))
  ids <- seq_len(nrow(embs))
  edges <- outer(
    ids,
    ids,
    Vectorize(\(n, m) {
      sub <- embs[n, ]
      sup <- embs[m, ]
      eq <- (is.na(sub) & is.na(sup)) | sub == sup
      sum(!eq, na.rm = TRUE) == 0 &&
        sum(eq, na.rm = TRUE) == length(sub) - 1 &&
        !is.na(sub[[which(is.na(eq))]]) &&
        is.na(sup[[which(is.na(eq))]])
    })
  ) |>
    which(arr.ind = TRUE) |>
    `colnames<-`(c("child", "parent"))
  edge_attrs <- Map(
    \(ch, pa) {
      sub <- embs[ch, ]
      sup <- embs[pa, ]
      eq <- (is.na(sub) & is.na(sup)) | sub == sup
      names(eq)[is.na(eq) | !eq]
    },
    edges[, "child"],
    edges[, "parent"]
  )
  enms <- apply(
    embs,
    1,
    \(x) {
      tokens <- ifelse(is.na(x), NA_character_, paste0(ifelse(x, "", "¬"), nms))
      paste0("[", toString(tokens[!is.na(tokens)]), "]")
    }
  )
  list(
    V = embs,
    E = edges,
    A = edge_attrs,
    N = enms
  )
}

dot_emb <- function(x, name) {
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

prune_embeddings <- function(emb, rules) {
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

  rem <- rep(FALSE, length(emb$N))
  attrs <- colnames(emb$V)
  for (n in seq_along(emb$N)) {
    bools <- emb$V[n, ]
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
      replacement <- which(apply(emb$V, 1, identical, new_bools))
      stopifnot(length(replacement) == 1)
      rem[[n]] <- TRUE
      emb$E[emb$E == n] <- replacement
    }
  }
  keep <- which(!rem)
  emb$E[] <- match(emb$E, keep)
  edge_keep <- apply(emb$E, 1, Negate(anyNA))
  emb$E <- emb$E[edge_keep, , drop = FALSE]
  emb$A <- emb$A[edge_keep]
  edge_nonself <- with(emb, E[, "child"] != E[, "parent"])
  emb$E <- emb$E[edge_nonself, , drop = FALSE]
  emb$A <- emb$A[edge_nonself]
  Elst <- if (nrow(emb$E) == 0)
    list()
  else
    apply(emb$E, 1, identity, simplify = FALSE)
  mats <- match(Elst, Elst)
  uniq <- unique(mats)
  emb$A <- lapply(
    uniq,
    \(n) intersect(
      colnames(emb$V),
      unique(do.call(c, emb$A[mats == n]))
    )
  )
  emb$E <- emb$E[uniq, , drop = FALSE]
  emb$V <- emb$V[keep, , drop = FALSE]
  emb$N <- emb$N[keep]
  emb
}

discover_presence <- function(x, embeds) {
  pres <- to_presence(x)
  res <- lapply(
    setNames(
      seq_along(embeds$N),
      embeds$N
    ),
    \(n) {
      V <- embeds$V[n, ]
      Vrep <- t(replicate(nrow(x), V))
      used <- apply(is.na(Vrep) | (Vrep == pres), 1, all)
      emb_pres <- which(V)

      sample <- x[used, which(V), drop = FALSE]
      pres_sample <- pres[used, which(is.na(V)), drop = FALSE]

      discover(
        cbind(sample, pres_sample),
        exclude = names(pres_sample),
        dependants = names(pres_sample)
      )
    }
  )
  # remove anything already implied by ancestors
  for (n in seq_along(embeds$N)) {
    parents <- embeds$E[embeds$E[, "child"] == n, "parent"]
    while (length(parents) > 0) {
      for (p in parents) {
        comp <- outer(res[[n]], res[[p]], `>=`)
        res[[n]] <- res[[n]][apply(comp, 1, Negate(any))]
      }
      parents <- embeds$E[embeds$E[, "child"] %in% parents, "parent"]
    }
  }
  res
}

discover_embedded <- function(x, embeds) {
  pres <- to_presence(x)
  res <- lapply(
    setNames(
      seq_along(embeds$N),
      embeds$N
    ),
    \(n) {
      V <- embeds$V[n, ]
      Vrep <- t(replicate(nrow(x), V))
      used <- apply(is.na(Vrep) | (Vrep == pres), 1, all)
      emb_pres <- which(V)

      sample <- x[used, which(V), drop = FALSE]

      discover(sample)
    }
  )
  # remove anything already implied by ancestors
  for (n in seq_along(embeds$N)) {
    parents <- embeds$E[embeds$E[, "child"] == n, "parent"]
    while (length(parents) > 0) {
      for (p in parents) {
        comp <- outer(res[[n]], res[[p]], `>=`)
        res[[n]] <- res[[n]][apply(comp, 1, Negate(any))]
      }
      parents <- embeds$E[embeds$E[, "child"] %in% parents, "parent"]
    }
  }
  res
}

prekey_schemas <- function(gefds, ...) {
  Map(
    \(x, nm) {
      res <- normalise(x, ensure_lossless = FALSE, ...)
      names(res) <- paste0(nm, "::", names(res), recycle0 = TRUE)
      res
    },
    gefds,
    names(gefds)
  )
}

add_partitions <- function(embed_schemas, pfds, embeds) {
  full_pfds <- pfds
  # fill with ancestors to ensure correct partition joins
  for (n in seq_along(embeds$N)) {
    nm <- embeds$N[[n]]
    parents <- embeds$E[embeds$E[, "child"] == n, "parent"]
    full_pfds[[n]] <- unique(Reduce(c, full_pfds[parents], full_pfds[[n]]))
    while (length(parents) > 0) {
      parents <- embeds$E[embeds$E[, "child"] %in% parents, "parent"]
      full_pfds[[n]] <- unique(Reduce(
        c,
        lapply(
          full_pfds[parents],
          \(fds) fds[!is.element(dependant(fds), names(!is.na(embeds$V[n, ])))]
        ),
        full_pfds[[n]]
      ))
    }
  }

  parts <- data.frame(
    embed = rep(names(full_pfds), lengths(full_pfds))
  )
  parts$key <- unlist(lapply(full_pfds, detset), recursive = FALSE)
  parts$dep <- Reduce(c, lapply(full_pfds, dependant), init = character())
  parts$children <- Map(
    \(x, dep) with(embeds, {
      names(full_pfds)[
        E[, "child"][
          E[, "parent"] == match(x, names(full_pfds)) &
            vapply(embeds$A, is.element, logical(1), el = dep)
        ]
      ]
    }),
    parts$embed,
    parts$dep
  )

  # make new key table if needed, create new partition reference either way
  new_refs <- list()
  for (n in seq_len(nrow(parts))) {
    key <- parts[[n, "key"]]
    parent <- parts[n, "embed"]
    ks <- keys(embed_schemas[[parent]])
    in_keys <- vapply(
      ks,
      \(x) any(vapply(x, identical, logical(1), key)),
      logical(1)
    )
    if (any(in_keys)) {
      stopifnot(sum(in_keys) == 1)
      parent_rel <- names(ks)[in_keys]
    }else{
      new_relname <- paste0(parent, "::key_", n)
      embed_schemas[[parent]] <- c(
        embed_schemas[[parent]],
        database_schema(
          relation_schema(
            setNames(
              list(list(key, list(key))),
              new_relname
            ),
            attrs_order(embed_schemas[[parent]])
          ),
          list()
        )
      )
      parent_rel <- new_relname
    }
    child_rels <- character()
    for (ch in parts[[n, "children"]]) {
      ks <- keys(embed_schemas[[ch]])
      in_keys <- vapply(
        ks,
        \(x) any(vapply(x, identical, logical(1), key)),
        logical(1)
      )
      if (any(in_keys)) {
        stopifnot(sum(in_keys) == 1)
        child_rels <- c(child_rels, names(ks)[in_keys])
      }else{
        new_relname <- paste0(ch, "::key_", n)
        embed_schemas[[ch]] <- c(
          embed_schemas[[ch]],
          database_schema(
            relation_schema(
              setNames(
                list(list(key, list(key))),
                new_relname
              ),
              attrs_order(embed_schemas[[ch]])
            ),
            list()
          )
        )
        child_rels <- c(child_rels, new_relname)
      }
    }
    new_refs <- c(
      new_refs,
      lapply(
        child_rels,
        \(ch) list(
          ch,
          key,
          parent_rel,
          key
        )
      )
    )
  }

  augmented_schemas <- lapply(
    embed_schemas,
    \(x) {
      x <- autoref(x)
      attrs_order(x) <- colnames(embeds$V)
      x
    }
  )
  list(
    schemas = augmented_schemas,
    attrs_order = colnames(embeds$V),
    interrefs = new_refs
  )
}

collapse_schemas <- function(x) {
  res <- Reduce(
    f = c,
    x$schemas,
    init = database_schema(
      relation_schema(
        setNames(list(), character()),
        x$attrs_order
      ),
      list()
    )
  )
  references(res) <- c(references(res), x$interrefs)
  res
}

decompose_embedded <- function(x, schema, embeds) {
  # insert parents first
  refs <- references(schema)
  ref_embeds <- vapply(
    refs,
    \(ref) vapply(strsplit(c(ref[[1]], ref[[3]]), "::"), `[[`, character(1), 1),
    character(2)
  )
  ref_embeds[] <- match(ref_embeds, embeds$N)
  ref_embeds <- ref_embeds[, ref_embeds[1, ] != ref_embeds[2, ], drop = FALSE]
  queue <- seq_along(embeds$N)
  db <- create(schema)
  while (length(queue) > 0) {
    candidates <- queue[!is.element(queue, ref_embeds[1, ])] # not a child
    stopifnot(length(candidates) > 0)
    n <- candidates[[1]]
    queue <- queue[queue != n]
    ref_embeds <- ref_embeds[, ref_embeds[2, ] != n, drop = FALSE] # remove as parent

    pattern <- embeds$V[n, ]
    sample <- x[
      apply(x[which(pattern)], 1, \(x) all(!is.na(x))) &
        apply(x[which(!pattern)], 1, \(x) all(is.na(x))),
      which(pattern),
      drop = FALSE
    ]
    db <- insert(
      db,
      sample,
      relations = names(db)[startsWith(names(db), paste0(embeds$N[[n]], "::"))]
    )
  }
  db
}
