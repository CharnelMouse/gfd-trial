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
  x[] <- lapply(x, factor, c(FALSE, TRUE))
  DATAFRAME(apriori(
    transactions(x),
    parameter = list(
      support = 1/nrow(x),
      confidence = 1
    )
  ))
}

fds_from_rules <- function(x, attrs_order) {
  lhs <- extract_rule_pairs(x$LHS)
  rhs <- extract_rule_pairs(x$RHS)
  lhs_logs <- mapply(to_attr_logs, lhs[[1]], lhs[[2]])
  rhs_logs <- mapply(to_attr_logs, rhs[[1]], rhs[[2]])
  functional_dependency(
    Map(
      list,
      lhs_logs,
      rhs_logs
    ),
    c(attrs_order, paste0("¬", attrs_order))
  )
}

extract_rule_pairs <- function(x) {
  pairlists <- gsub("[{}]", "", as.character(x)) |>
    strsplit(",") |>
    lapply(\(x) {
      do.call(
        Map,
        c(
          list(c),
          strsplit(x, "=")
        )
      ) |>
        (\(y) {
          if (length(y) == 0) {
            list(character(), logical())
          }else{ list(y[[1]], as.logical(y[[2]]))
          }
        })()
    })
  list(
    lapply(pairlists, `[[`, 1),
    lapply(pairlists, `[[`, 2)
  )
}

to_attr_logs <- function(ch, lg) {
  if (length(ch) == 0)
    return(character())
  ifelse(lg, ch, paste0("¬", ch))
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
  embedding(
    V = embs,
    E = edges,
    A = edge_attrs,
    N = enms
  )
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
  remove_ancestor_fds(res, embeds)
}

discover_embedded <- function(x, embeds, ...) {
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

      discover(sample, ...)
    }
  )
  remove_ancestor_fds(res, embeds)
}

remove_ancestor_fds <- function(res, embeds) {
  # remove anything already implied by ancestors
  for (n in seq_along(embeds$N)) {
    parents <- parents(embeds, n)
    while (length(parents) > 0) {
      for (p in parents) {
        comp <- outer(res[[n]], res[[p]], `>=`)
        res[[n]] <- res[[n]][apply(comp, 1, Negate(any))]
      }
      parents <- parents(embeds, parents)
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
    parents <- parents(embeds, n)
    full_pfds[[n]] <- unique(Reduce(c, full_pfds[parents], full_pfds[[n]]))
    while (length(parents) > 0) {
      parents <- parents(embeds, parents)
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
    key_txt <- autodb:::make.gv_names(paste(key, collapse = "_"))
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
      new_relname <- paste0(parent, "::", key_txt)
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
        new_relname <- paste0(ch, "::", key_txt)
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

gv_embed <- function(x, embeds) {
  main_gv <- strsplit(gv(x), "\n")[[1]]
  dbs <- split(
    x,
    strsplit(names(x), "::") |> vapply(`[[`, character(1), 1)
  )
  gvs <- lapply(dbs, \(x) strsplit(gv(x), "\n")[[1]])
  subgvs <- lapply(
    setNames(nm = names(gvs)),
    \(nm) {
      x <- gvs[[nm]]
      x <- sub("^digraph", paste0("subgraph cluster_", match(nm, names(gvs))), x)
      x <- gsub(">\\[[^]]+\\]::", ">", x)
      x <- append(x, paste0("  label = \"", nm, "\""), after = 1)
      x <- append(x, paste0("  color = lightgrey"), after = 2)
      x
    }
  )
  gv_clusters <- Reduce(c, subgvs, init = character())
  rem_gv <- setdiff(main_gv, gv_clusters) |>
    grep(pattern = "<TR>", value = TRUE, invert = TRUE)
  rem_gv <- gsub(">\\[[^]]+\\]::", ">", rem_gv)
  paste(
    c(
      rem_gv[1],
      "  rankdir = \"LR\"",
      paste0("  ", gv_clusters),
      rem_gv[-1],
      "}"
    ),
    collapse = "\n"
  )
}
