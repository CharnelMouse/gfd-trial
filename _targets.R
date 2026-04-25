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
    add_partitions(prekey_schema2, pfds2, embed2)
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
    gv(nullfree_db2)
  ),

  tar_target(
    ex4,
    data.frame(
      id = 1:20,
      value = c(2.3, 2.3, 5.7, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_),
      lower_bound = c(NA_real_, NA_real_, NA_real_, 2.4, 0, 1, 0, 5.6, 2.4, 5.3, 5.3, 2.4, 2.4, 2.4, 2.4, 2.4, 2.4, 2.4, 5.6, 2.4),
      upper_bound = c(NA_real_, NA_real_, NA_real_, 7.1, 10, 10, 13.1, 25.8, 10, 13.1, 10, 25.8, 25.8, 25.8, 25.8,13.1, 13.1, 25.8, 25.8, 25.8),
      interval_distribution = factor(c(NA, NA, NA, "uniform", "uniform", "uniform", "uniform", "uniform", "Beta", "Beta", "Beta", "Beta", "Kumaraswamy", "Kumaraswamy", "Kumaraswamy", "Kumaraswamy", "PERT", "PERT", "PERT", "PERT")),
      param1 = c(NA, NA, NA, NA, NA, NA, NA, NA, 1, 1, 1, 2, 2, 2.1, 2, 2, 2, 1, 2, 2),
      param2 = c(NA, NA, NA, NA, NA, NA, NA, NA, 1, 2, 2, 2, 2, 1, 1, 1, NA, NA, NA, NA)
    )
  ),
  tar_target(
    presence4,
    to_presence(ex4)
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
    embeddings(ex4)
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
    discover_presence(ex4, embed4)
  ),
  tar_target(
    gefds4,
    discover_embedded(ex4, embed4)
  ),

  tar_target(
    prekey_schema4,
    prekey_schemas(gefds4, remove_avoidable = TRUE)
  ),
  tar_target(
    nullfree_schema4,
    add_partitions(prekey_schema4, pfds4, embed4)
  ),
  tar_target(
    nullfree_gv4,
    gv(nullfree_schema4)
  ),
  tar_target(
    nullfree_db4,
    decompose_embedded(ex4, nullfree_schema4, embed4)
  ),
  tar_target(
    nullfree_dbgv4,
    gv(nullfree_db4)
  ),

  tar_quarto(
    report,
    "report.qmd",
    quiet = FALSE
  )
)
