library(targets)
library(tarchetypes)
library(autodb)

tar_source()

list(
  tar_target(
    ex1,
    data.frame(a = 1:8, b = 1:4, c = 1:2)
  ),
  tar_target(
    fds1,
    discover(ex1)
  ),
  tar_target(
    schema1,
    normalise(fds1)
  ),
  tar_target(
    db1,
    decompose(ex1, schema1)
  ),
  tar_target(
    gv1,
    gv(db1)
  ),

  tar_target(
    ex2,
    data.frame(
      id = 1:7,
      start = c(3, 3, 3, 3, 6, 6, 6),
      end = c(5, 7, NA, 5, 7, NA, NA)
    )
  ),
  tar_target(
    db2,
    autodb(ex2)
  ),
  tar_target(
    gv2,
    gv(db2)
  ),
  tar_target(
    db_ideal2,
    {
      schema <- database_schema(
        relation_schema(
          list(
            interval = list(c("id", "start"), list("id")),
            `finished interval` = list(c("id", "end"), list("id"))
          ),
          names(ex2)
        ),
        list(list("finished interval", "id", "interval", "id"))
      )
      db <- decompose(ex2, schema)
      records(db)$`finished interval` <- subset(
        records(db)$`finished interval`,
        !is.na(end)
      )
      db
    }
  ),
  tar_target(
    gv_ideal2,
    gv(db_ideal2)
  ),

  tar_target(
    presence2,
    to_presence(ex2)
  ),
  tar_target(
    rules2,
    find_rules(presence2),
    packages = "arules"
  ),
  tar_target(
    rulefds2,
    fds_from_rules(rules2, names(presence2))
  ),
  tar_target(
    minrulefds2,
    minimise_rulefds(rulefds2)
  ),

  tar_target(
    ex3,
    subset(
      expand.grid(a = c(F, T), b = c(F, T), c = c(F, T), d = c(F, T)),
      (!a | b) & (!a | !c) & (a | d)
    )
  ),
  tar_target(
    rules3,
    find_rules(ex3),
    packages = "arules"
  ),
  tar_target(
    rulefds3,
    fds_from_rules(rules3, names(ex3))
  ),
  tar_target(
    minrulefds3,
    minimise_rulefds(rulefds3)
  ),
  tar_target(
    dpres3,
    cbind(
      ex3,
      setNames(
        as.data.frame(!ex3, check.names = FALSE),
        paste0("¬", names(ex3))
      )
    )
  ),
  tar_target(
    drules3,
    find_rules(dpres3),
    packages = "arules"
  ),
  tar_target(
    drulefds3,
    fds_from_rules(drules3, names(dpres3))
  ),
  tar_target(
    mindrulefds3,
    minimise_rulefds(drulefds3) |>
      sort_fds()
  ),
  tar_target(
    minntdrulefds3,
    remove_extraneous(mindrulefds3)
  ),
  tar_target(
    grpfds3,
    group_fds(mindrulefds3)
  ),

  tar_target(
    all_embed2,
    embeddings(ex2)
  ),
  tar_target(
    dot_all_embed2,
    dot_emb(all_embed2, "all_embed2")
  ),
  tar_target(
    dpres2,
    cbind(
      presence2,
      setNames(
        as.data.frame(!presence2, check.names = FALSE),
        paste0("¬", names(presence2))
      )
    )
  ),
  tar_target(
    drules2,
    find_rules(dpres2),
    packages = "arules"
  ),
  tar_target(
    drulefds2,
    fds_from_rules(drules2, names(dpres2))
  ),
  tar_target(
    mindrulefds2,
    minimise_rulefds(drulefds2)
  ),
  tar_target(
    embed2,
    prune_embeddings(all_embed2, mindrulefds2)
  ),
  tar_target(
    dot_embed2,
    dot_emb(embed2, "embed2")
  ),

  tar_target(
    embed3,
    prune_embeddings(embeddings(ex3), mindrulefds3)
  ),
  tar_target(
    dot_embed3,
    dot_emb(embed3, "embed3")
  ),

  tar_target(
    pfds2,
    discover_presence(ex2, embed2)
  ),
  tar_target(
    gefds2,
    discover_embedded(ex2, embed2)
  ),

  tar_target(
    prekey_schema2,
    prekey_schemas(gefds2, remove_avoidable = TRUE)
  ),
  tar_target(
    nullfree_schema2,
    add_partitions(prekey_schema2, pfds2, embed2) |>
      collapse_schemas()
  ),
  tar_target(
    nullfree_gv2,
    gv(nullfree_schema2)
  ),
  tar_target(
    nullfree_db2,
    decompose_embedded(ex2, nullfree_schema2, embed2)
  ),
  tar_target(
    nullfree_dbgv2,
    gv_embed(nullfree_db2, embed2)
  ),
  tar_target(
    prunedembed2,
    remove_vacuous_embeddings(embed2, gefds2)
  ),
  tar_target(
    pruned_pfds2,
    pfds2[names(pfds2) %in% prunedembed2$N]
  ),
  tar_target(
    pruned_gefds2,
    gefds2[names(gefds2) %in% prunedembed2$N]
  ),
  tar_target(
    prekeyprune_schema2,
    prekey_schemas(pruned_gefds2, remove_avoidable = TRUE)
  ),
  tar_target(
    nullfreeprune_schema2,
    add_partitions(prekeyprune_schema2, pruned_pfds2, prunedembed2) |>
      collapse_schemas()
  ),
  tar_target(
    nullfreeprune_db2,
    decompose_embedded(ex2, nullfreeprune_schema2, prunedembed2)
  ),
  tar_target(
    nullfreeprune_dbgv2,
    gv_embed(nullfreeprune_db2, prunedembed2)
  ),

  tar_target(
    ex4,
    data.frame(
      id = 1:24,
      value = c(2.3, 2.3, 5.7, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_),
      lower_bound = c(NA_real_, NA_real_, NA_real_, 2.4, 0, 1, 0, 5.6, 0, 5.6, 2.4, 5.3, 5.3, 2.4, 2.4, 2.4, 2.4, 2.4, 2.4, 2.4, 5.6, 2.4, 5.6, 2.4),
      upper_bound = c(NA_real_, NA_real_, NA_real_, 7.1, 10, 10, 13.1, 25.8, 13.1, 25.8, 10, 13.1, 10, 25.8, 25.8, 25.8, 25.8,13.1, 13.1, 25.8, 25.8, 25.8, 25.8, 25.8),
      interval_distribution = factor(c(NA, NA, NA, "uniform", "uniform", "uniform", "uniform", "uniform", "arcsin", "arcsin", "Beta", "Beta", "Beta", "Beta", "Kumaraswamy", "Kumaraswamy", "Kumaraswamy", "Kumaraswamy", "PERT", "PERT", "PERT", "PERT", "Wigner", "Wigner")),
      param1 = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 1, 1, 1, 2, 2, 2.1, 2, 2, 2, 1, 2, 2, 2, 2),
      param2 = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 1, 2, 2, 2, 2, 1, 1, 1, NA, NA, NA, NA, NA, NA)
    )
  ),
  tar_target(
    short4,
    setNames(
      ex4,
      c("i", "v", "l", "u", "d", "p1", "p2")
    )
  ),
  tar_target(
    presence4,
    to_presence(short4)
  ),
  tar_target(
    dpres4,
    cbind(
      presence4,
      setNames(
        as.data.frame(!presence4, check.names = FALSE),
        paste0("¬", names(presence4))
      )
    )
  ),
  tar_target(
    drules4,
    find_rules(dpres4),
    packages = "arules"
  ),
  tar_target(
    drulefds4,
    fds_from_rules(drules4, names(dpres4))
  ),
  tar_target(
    mindrulefds4,
    minimise_rulefds(drulefds4)
  ),
  tar_target(
    all_embed4,
    embeddings(short4)
  ),
  tar_target(
    embed4,
    prune_embeddings(all_embed4, mindrulefds4)
  ),
  tar_target(
    dot_embed4,
    dot_emb(embed4, "embed2")
  ),

  tar_target(
    pfds4,
    discover_presence(short4, embed4)
  ),
  tar_target(
    gefds4,
    discover_embedded(short4, embed4, exclude = setdiff(names(short4), c("i", "d")))
  ),

  tar_target(
    prekey_schema4,
    prekey_schemas(gefds4, remove_avoidable = TRUE)
  ),
  tar_target(
    nullfree_schema4,
    add_partitions(prekey_schema4, pfds4, embed4) |>
      collapse_schemas()
  ),
  tar_target(
    nullfree_gv4,
    gv(nullfree_schema4)
  ),
  tar_target(
    nullfree_db4,
    decompose_embedded(short4, nullfree_schema4, embed4)
  ),
  tar_target(
    nullfree_dbgv4,
    gv_embed(nullfree_db4, embed4)
  ),

  tar_target(
    prunedembed4,
    remove_vacuous_embeddings(embed4, gefds4)
  ),
  tar_target(
    pruned_pfds4,
    pfds4[names(pfds4) %in% prunedembed4$N]
  ),
  tar_target(
    pruned_gefds4,
    gefds4[names(gefds4) %in% prunedembed4$N]
  ),
  tar_target(
    prekeyprune_schema4,
    prekey_schemas(pruned_gefds4, remove_avoidable = TRUE)
  ),
  tar_target(
    nullfreeprune_schema4,
    add_partitions(prekeyprune_schema4, pruned_pfds4, prunedembed4) |>
      collapse_schemas()
  ),
  tar_target(
    nullfreeprune_db4,
    decompose_embedded(short4, nullfreeprune_schema4, prunedembed4)
  ),
  tar_target(
    nullfreeprune_dbgv4,
    gv_embed(nullfreeprune_db4, prunedembed4)
  ),

  tar_target(
    ex5,
    data.frame(
      a = 1:12,
      b = 1:4,
      c = c(1, 2, NA, NA, 2, 1, NA, NA, 3, 1, NA, NA)
    )
  ),
  tar_target(
    presence5,
    to_presence(ex5)
  ),
  tar_target(
    dpres5,
    cbind(
      presence5,
      setNames(
        as.data.frame(!presence5, check.names = FALSE),
        paste0("¬", names(presence5))
      )
    )
  ),
  tar_target(
    drules5,
    find_rules(dpres5),
    packages = "arules"
  ),
  tar_target(
    drulefds5,
    fds_from_rules(drules5, names(dpres5))
  ),
  tar_target(
    mindrulefds5,
    minimise_rulefds(drulefds5)
  ),
  tar_target(
    all_embed5,
    embeddings(ex5)
  ),
  tar_target(
    embed5,
    prune_embeddings(all_embed5, mindrulefds5)
  ),
  tar_target(
    dot_embed5,
    dot_emb(embed5, "embed2")
  ),

  tar_target(
    pfds5,
    discover_presence(ex5, embed5)
  ),
  tar_target(
    gefds5,
    discover_embedded(ex5, embed5)
  ),

  tar_target(
    prekey_schema5,
    prekey_schemas(gefds5, remove_avoidable = TRUE)
  ),
  tar_target(
    nullfree_schema5,
    add_partitions(prekey_schema5, pfds5, embed5) |>
      collapse_schemas()
  ),
  tar_target(
    nullfree_gv5,
    gv(nullfree_schema5)
  ),
  tar_target(
    nullfree_db5,
    decompose_embedded(ex5, nullfree_schema5, embed5)
  ),
  tar_target(
    nullfree_dbgv5,
    gv(nullfree_db5)
  ),

  tar_target(
    pfdstrim5,
    {
      res <- pfds5
      res$`[a, b]` <- res$`[a, b]`[!vapply(
        detset(res$`[a, b]`),
        identical,
        logical(1),
        "a"
      )]
      res
    }
  ),
  tar_target(
    nullfreetrim_schema5,
    add_partitions(prekey_schema5, pfdstrim5, embed5) |>
      collapse_schemas()
  ),
  tar_target(
    nullfreetrim_gv5,
    gv(nullfreetrim_schema5)
  ),
  tar_target(
    nullfreetrim_db5,
    decompose_embedded(ex5, nullfreetrim_schema5, embed5)
  ),
  tar_target(
    nullfreetrim_dbgv5,
    gv_embed(nullfreetrim_db5, embed5)
  ),

  tar_target(
    prunedembed5,
    remove_vacuous_embeddings(embed5, gefds5)
  ),
  tar_target(
    pruned_pfds5,
    pfds5[names(pfds5) %in% prunedembed5$N]
  ),
  tar_target(
    pruned_gefds5,
    gefds5[names(gefds5) %in% prunedembed5$N]
  ),
  tar_target(
    prekeyprune_schema5,
    prekey_schemas(pruned_gefds5, remove_avoidable = TRUE)
  ),
  tar_target(
    nullfreeprune_schema5,
    add_partitions(prekeyprune_schema5, pruned_pfds5, prunedembed5) |>
      collapse_schemas()
  ),
  tar_target(
    nullfreeprune_db5,
    decompose_embedded(ex5, nullfreeprune_schema5, prunedembed5)
  ),
  tar_target(
    nullfreeprune_dbgv5,
    gv_embed(nullfreeprune_db5, prunedembed5)
  ),

  tar_target(
    goal4,
    "goal4.dot",
    format = "file"
  ),

  tar_quarto(
    report,
    "report.qmd",
    quiet = FALSE
  )
)
