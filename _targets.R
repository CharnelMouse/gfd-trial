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
    db2,
    autodb(data.ex2)
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
          names(data.ex2)
        ),
        list(list("finished interval", "id", "interval", "id"))
      )
      db <- decompose(data.ex2, schema)
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
    minntdrulefds3,
    remove_extraneous(minimal_presence_rule_fds.ex3)
  ),
  tar_target(
    grpfds3,
    group_fds(minimal_presence_rule_fds.ex3)
  ),

  tar_map(
    values = list(
      df = list(
        ex2 = data.frame(
          id = 1:7,
          start = c(3, 3, 3, 3, 6, 6, 6),
          end = c(5, 7, NA, 5, 7, NA, NA)
        ),
        ex3 = subset(
          expand.grid(a = c(F, T), b = c(F, T), c = c(F, T), d = c(F, T)),
          (!a | b) & (!a | !c) & (a | d)
        ) |>
          (\(x) {x[] <- lapply(x, \(y) ifelse(y, T, NA)); x})(),
        ex4 = data.frame(
          id = 1:24,
          value = c(2.3, 2.3, 5.7, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_),
          lower_bound = c(NA_real_, NA_real_, NA_real_, 2.4, 0, 1, 0, 5.6, 0, 5.6, 2.4, 5.3, 5.3, 2.4, 2.4, 2.4, 2.4, 2.4, 2.4, 2.4, 5.6, 2.4, 5.6, 2.4),
          upper_bound = c(NA_real_, NA_real_, NA_real_, 7.1, 10, 10, 13.1, 25.8, 13.1, 25.8, 10, 13.1, 10, 25.8, 25.8, 25.8, 25.8,13.1, 13.1, 25.8, 25.8, 25.8, 25.8, 25.8),
          distribution = factor(c(NA, NA, NA, "uniform", "uniform", "uniform", "uniform", "uniform", "arcsin", "arcsin", "Beta", "Beta", "Beta", "Beta", "Kumaraswamy", "Kumaraswamy", "Kumaraswamy", "Kumaraswamy", "PERT", "PERT", "PERT", "PERT", "Wigner", "Wigner")),
          param1 = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 1, 1, 1, 2, 2, 2.1, 2, 2, 2, 1, 2, 2, 2, 2),
          param2 = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 1, 2, 2, 2, 2, 1, 1, 1, NA, NA, NA, NA, NA, NA)
        ),
        ex5 = data.frame(
          a = 1:12,
          b = 1:4,
          c = c(1, 2, NA, NA, 2, 1, NA, NA, 3, 1, NA, NA)
        ),
        ex6 = data.frame(
          a = 1:6,
          b = c(1, 1, 2, NA, NA, NA),
          c = c(NA, NA, NA, 1, 1, 2)
        ),
        ex7 = ChickWeight |>
          (\(x) {
            merge(
              expand.grid(Time = unique(x$Time), Chick = unique(x$Chick)) |>
                merge(unique(ChickWeight[c("Chick", "Diet")])),
              x,
              all = TRUE
            )
          })(),
        ex8 = data.frame(
          a = c(1:3, NA, NA),
          b = c(1:2, 2L, 1:2)
        )
      ),
      shorten = c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE),
      exclude = list(
        character(),
        character(),
        c("v", "l", "u", "p1", "p2"),
        character(),
        character(),
        character(),
        character()
      ),
      exclude_class = list(
        character(),
        character(),
        character(),
        character(),
        character(),
        character(),
        character()
      ),
      nm = c("ex2", "ex3", "ex4", "ex5", "ex6", "ex7", "ex8")
    ),
    names = "nm",
    delimiter = ".",

    tar_target(
      data,
      df
    ),
    tar_target(
      short,
      if (!shorten)
        data
      else
        setNames(
          data,
          make.unique(sub(
            "^(\\w)\\D*(\\d*)",
            "\\1\\2",
            names(data),
            perl = TRUE
          ))
        )
    ),

    # dependency search

    tar_target(
      presence,
      to_presence(short)
    ),
    tar_target(
      presence_rules,
      find_rules(presence),
      packages = "arules"
    ),
    tar_target(
      presence_rule_fds,
      fds_from_rules(presence_rules, names(presence))
    ),
    tar_target(
      count_presence_rule_fds,
      length(presence_rule_fds)
    ),
    tar_target(
      minimal_presence_rule_fds,
      minimise_rulefds(presence_rule_fds, progress = TRUE)
    ),
    tar_target(
      count_minimal_presence_rule_fds,
      length(minimal_presence_rule_fds)
    ),
    tar_target(
      all_embeddings,
      embeddings(short)
    ),
    tar_target(
      dot_all_embeddings,
      gv_embedding(all_embeddings)
    ),
    tar_target(
      searched_embeddings,
      prune_embedding(all_embeddings, minimal_presence_rule_fds, progress = TRUE)
    ),
    tar_target(
      dot_searched_embeddings,
      gv_embedding(searched_embeddings)
    ),
    tar_target(
      presence_fds,
      discover_presence(short, searched_embeddings, progress = TRUE)
    ),
    tar_target(
      gefds,
      discover_embedded(
        short,
        searched_embeddings,
        progress = TRUE,
        exclude = exclude,
        exclude_class = exclude_class
      )
    ),

    # schema generation

    tar_target(
      prekey_schema,
      prekey_schemas(gefds, remove_avoidable = TRUE)
    ),
    tar_target(
      nullfree_schema,
      add_partitions(prekey_schema, presence_fds, searched_embeddings, progress = TRUE) |>
        collapse_schemas()
    ),
    tar_target(
      gv_nullfree_schema,
      gv_embed(nullfree_schema)
    ),
    tar_target(
      nullfree_db,
      decompose_embedded(short, nullfree_schema)
    ),
    tar_target(
      gv_nullfree_db,
      gv_embed(nullfree_db)
    ),

    # pruned version with key-only embeddings removed
    tar_target(
      pruned_nullfree_schema,
      prune_nullfree_schema(nullfree_schema)
    ),
    tar_target(
      pruned_nullfree_db,
      decompose_embedded(short, pruned_nullfree_schema)
    ),
    tar_target(
      gv_pruned_nullfree_db,
      gv_embed(pruned_nullfree_db)
    )
  ),

  tar_target(
    pfdstrim5,
    {
      res <- presence_fds.ex5
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
    add_partitions(prekey_schema.ex5, pfdstrim5, searched_embeddings.ex5) |>
      collapse_schemas()
  ),
  tar_target(
    nullfreetrim_db5,
    decompose_embedded(data.ex5, nullfreetrim_schema5)
  ),
  tar_target(
    nullfreetrim_dbgv5,
    gv_embed(nullfreetrim_db5)
  ),

  tar_quarto(
    report,
    "report.qmd",
    quiet = FALSE
  )
)
