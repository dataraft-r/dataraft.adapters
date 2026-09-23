#' Exchange an Open Data Contract Standard 3.2 document
#'
#' Export a single flat table to ODCS 3.2.0, or import its executable subset.
#' Import never evaluates R code from a document. Unsupported nested types,
#' constraints and quality engines are rejected. Retained document metadata is
#' descriptive, not an assertion that roles, SLAs or retention are enforced.
#' Native formula exports use a DataRaft custom engine; other ODCS engines need
#' an implementation of that engine to execute these rules.
#' @param contract A DataRaft contract.
#' @param path Optional JSON or YAML output path (YAML needs package yaml).
#' @param x An ODCS list or a JSON/YAML file path.
#' @returns An ODCS list, invisibly when written, or a DataRaft contract.
#' @export
dr_contract_odcs <- function(contract, path = NULL) {
  if (!inherits(contract, "dr_contract")) {
    stop("Supply a DataRaft contract.")
  }
  types <- c(
    character = "string",
    integer = "integer",
    numeric = "number",
    logical = "boolean",
    Date = "date",
    POSIXct = "timestamp",
    integer64 = "integer"
  )
  if (any(!unlist(contract$columns) %in% names(types))) {
    stop(
      "ODCS export requires flat, typed columns; list columns need an explicit nested schema."
    )
  }
  original <- contract$governance$odcs %||% list()
  properties <- lapply(names(contract$columns), function(name) {
    old <- Filter(
      function(p) identical(p$name, name),
      original$schema[[1]]$properties %||% list()
    )
    p <- if (length(old)) old[[1]] else list()
    p$quality <- NULL
    p$name <- name
    p$logicalType <- unname(types[[contract$columns[[name]]]])
    p$logicalTypeOptions <- NULL
    p$required <- name %in% contract$required
    p$primaryKey <- name %in% contract$key
    if (p$primaryKey) {
      p$primaryKeyPosition <- match(name, contract$key)
    } else {
      p$primaryKeyPosition <- NULL
    }
    if (contract$columns[[name]] == "integer64") {
      p$logicalTypeOptions <- list(format = "i64")
    }
    meta <- contract$column_metadata[[name]] %||% list()
    for (field in intersect(
      names(meta),
      c("description", "classification", "tags", "businessName")
    )) {
      p[[field]] <- meta[[field]]
    }
    p
  })
  rules <- lapply(contract$rules, function(rule) {
    if (!is.null(rule$odcs)) {
      return(rule$odcs)
    }
    if (!inherits(rule$check, "formula") || !identical(rule$engine, "r")) {
      stop(
        "Only native formula rules can be exported to the DataRaft ODCS engine."
      )
    }
    expression <- paste(
      deparse(rule$check[[2]], width.cutoff = 500L),
      collapse = " "
    )
    safe_odcs_formula(expression, names(contract$columns))
    item <- list(
      name = rule$name,
      type = "custom",
      engine = "dataraft",
      severity = if (identical(rule$action, "warn")) "warning" else "error",
      implementation = list(
        predicate = expression,
        threshold = rule$threshold,
        action = rule$action
      )
    )
    if (!is.null(rule$dimension)) {
      item$dimension <- rule$dimension
    }
    item
  })
  out <- original
  out$apiVersion <- "v3.2.0"
  out$kind <- "DataContract"
  out$id <- original$id %||% contract$id
  out$version <- contract$version
  out$name <- original$name %||% contract$id
  out$description$purpose <- contract$description
  object <- original$schema[[1]] %||% list()
  object$name <- original$schema[[1]]$name %||% contract$id
  object$logicalType <- "object"
  object$dataGranularityDescription <- contract$grain
  object$properties <- properties
  object$quality <- if (length(rules)) rules else NULL
  out$schema <- list(object)
  if (nzchar(contract$owner)) {
    out$team$name <- contract$owner
  }
  for (field in c("tags", "roles", "slaProperties")) {
    value <- contract$governance[[
      if (field == "slaProperties") "sla" else field
    ]]
    if (!is.null(value)) out[[field]] <- unname(as.list(value))
  }
  policy <- contract$governance
  policy$odcs <- NULL
  out$customProperties <- Filter(
    function(p) !identical(p$property, "dataraft"),
    out$customProperties %||% list()
  )
  out$customProperties <- c(
    out$customProperties,
    list(list(
      property = "dataraft",
      value = Filter(
        Negate(is.null),
        list(
          allow_empty = contract$allow_empty,
          allow_extra = contract$allow_extra,
          producer = contract$producer,
          max_age_hours = contract$max_age_hours,
          operator = contract$operator,
          column_metadata = contract$column_metadata,
          governance = policy
        )
      )
    ))
  )
  if (!is.null(path)) {
    if (grepl("[.]json$", path, ignore.case = TRUE)) {
      jsonlite::write_json(out, path, auto_unbox = TRUE, pretty = TRUE)
    } else {
      dataraft.core::dr_internal_need("yaml")
      yaml::write_yaml(out, path)
    }
    return(invisible(out))
  }
  out
}

#' @rdname dr_contract_odcs
#' @export
dr_contract_from_odcs <- function(x) {
  if (is.character(x) && length(x) == 1L) {
    if (grepl("[.]json$", x, ignore.case = TRUE)) {
      x <- jsonlite::read_json(x, simplifyVector = FALSE)
    } else {
      dataraft.core::dr_internal_need("yaml")
      x <- yaml::read_yaml(x, eval.expr = FALSE)
    }
  }
  if (
    !is.list(x) ||
      !identical(x$apiVersion, "v3.2.0") ||
      !identical(x$kind, "DataContract") ||
      length(x$schema) != 1L
  ) {
    stop("Supply an ODCS v3.2.0 DataContract containing exactly one table.")
  }
  dataraft.core::dr_internal_scalar(x$id, "ODCS id")
  object <- x$schema[[1]]
  if (length(object$relationships)) {
    stop("ODCS relationships require a relational contract implementation.")
  }
  if (
    !is.null(object$logicalType) && !identical(object$logicalType, "object")
  ) {
    stop("ODCS table logicalType must be object.")
  }
  properties <- object$properties
  if (!length(properties)) {
    stop("ODCS schema needs properties.")
  }
  names <- vapply(properties, function(p) p$name, character(1))
  types <- c(
    string = "character",
    integer = "integer",
    number = "numeric",
    boolean = "logical",
    date = "Date",
    timestamp = "POSIXct"
  )
  columns <- vapply(
    properties,
    function(p) {
      if (!p$logicalType %in% names(types)) {
        stop("Unsupported ODCS logical type: ", p$logicalType)
      }
      if (length(p$enum) || length(p$relationships) || isTRUE(p$unique)) {
        stop(
          "Property quality, enum, relationships and unique constraints require an explicit implementation."
        )
      }
      options <- p$logicalTypeOptions %||% list()
      if (length(setdiff(names(options), "format"))) {
        stop(
          "ODCS logical type constraints are not executable by this importer."
        )
      }
      if (
        !is.null(options$format) &&
          !paste(p$logicalType, options$format) %in%
            c("integer i32", "integer i64", "number f64")
      ) {
        stop("Unsupported ODCS logical type format.")
      }
      if (
        identical(p$logicalType, "integer") && identical(options$format, "i64")
      ) {
        "integer64"
      } else {
        unname(types[[p$logicalType]])
      }
    },
    character(1)
  )
  names(columns) <- names
  all_rules <- object$quality %||% list()
  for (p in properties) {
    for (rule in p$quality %||% list()) {
      rule$arguments$column <- p$name
      all_rules[[length(all_rules) + 1L]] <- rule
    }
  }
  rules <- lapply(all_rules, function(rule) {
    if (identical(rule$type %||% "library", "library")) {
      return(odcs_library_rule(rule, names))
    }
    if (
      !identical(rule$type, "custom") ||
        !identical(rule$engine, "dataraft") ||
        !is.list(rule$implementation)
    ) {
      stop(
        "Unsupported ODCS quality engine. Import cannot silently discard quality rules."
      )
    }
    impl <- rule$implementation
    dataraft.core::dr_quality_rule(
      name = rule$name,
      check = safe_odcs_formula(impl$predicate, names),
      action = impl$action %||%
        if (identical(rule$severity, "warning")) "warn" else "block",
      threshold = impl$threshold %||% 0,
      dimension = rule$dimension
    )
  })
  vendor <- Filter(
    function(p) identical(p$property, "dataraft"),
    x$customProperties %||% list()
  )
  policy <- if (length(vendor)) vendor[[1]]$value else list()
  governance <- policy$governance %||% list()
  governance$odcs <- x
  governance$roles <- x$roles
  governance$sla <- x$slaProperties
  governance$tags <- x$tags
  key <- which(vapply(properties, function(p) isTRUE(p$primaryKey), logical(1)))
  if (length(key)) {
    key <- key[order(vapply(
      properties[key],
      function(p) p$primaryKeyPosition %||% 1,
      numeric(1)
    ))]
  }
  dataraft.core::dr_contract(
    id = if (
      is.character(x$id) &&
        length(x$id) == 1L &&
        !is.na(x$id) &&
        grepl("^[A-Za-z][A-Za-z0-9_.]*$", x$id)
    ) {
      x$id
    } else {
      paste0("odcs.", digest::digest(x$id, algo = "sha256", serialize = TRUE))
    },
    version = x$version,
    columns = columns,
    key = names[key],
    rules = rules
  ) |>
    dataraft.core::dr_contract_meta(
      owner = x$team$name %||% "",
      producer = policy$producer %||% x$team$name %||% "",
      description = x$description$purpose %||% "",
      grain = object$dataGranularityDescription %||% "",
      governance = governance,
      operator = policy$operator,
      column_metadata = stats::setNames(
        lapply(properties, function(p) {
          utils::modifyList(
            policy$column_metadata[[p$name]] %||% list(),
            p[intersect(
              names(p),
              c("description", "classification", "tags", "businessName")
            )]
          )
        }),
        names
      )
    ) |>
    dataraft.core::dr_contract_policy(
      required = names[vapply(
        properties,
        function(p) isTRUE(p$required),
        logical(1)
      )],
      allow_empty = policy$allow_empty %||% FALSE,
      allow_extra = policy$allow_extra %||% FALSE,
      max_age_hours = policy$max_age_hours
    )
}

safe_odcs_formula <- function(text, columns) {
  if (!is.character(text) || length(text) != 1L || is.na(text)) {
    stop("Supply one declarative predicate string.")
  }
  parsed <- parse(text = text, keep.source = FALSE)
  if (length(parsed) != 1L) {
    stop("A predicate must contain one expression.")
  }
  allowed <- c(
    "(",
    "!",
    "&",
    "|",
    "==",
    "!=",
    ">",
    ">=",
    "<",
    "<=",
    "+",
    "-",
    "*",
    "/",
    "is.na",
    "%in%",
    "c"
  )
  inspect <- function(node) {
    if (is.symbol(node)) {
      if (!as.character(node) %in% columns) {
        stop("Unknown predicate column: ", as.character(node))
      }
      return(invisible(NULL))
    }
    if (is.call(node)) {
      if (!is.symbol(node[[1]]) || !as.character(node[[1]]) %in% allowed) {
        stop(
          "Unsupported predicate operation. Arbitrary R code is never evaluated from ODCS."
        )
      }
      lapply(as.list(node)[-1], inspect)
    } else if (!is.atomic(node) || length(node) != 1L) {
      stop("Unsupported predicate value.")
    }
    invisible(NULL)
  }
  inspect(parsed[[1]])
  rlang::new_formula(NULL, parsed[[1]], env = baseenv())
}


odcs_library_rule <- function(rule, columns) {
  metric <- rule$metric
  if (!metric %in% c("nullValues", "rowCount")) {
    stop("Unsupported ODCS library metric.")
  }
  column <- rule$arguments$column
  if (
    metric != "rowCount" &&
      (!is.character(column) || length(column) != 1L || !column %in% columns)
  ) {
    stop("ODCS metric requires a known arguments.column.")
  }
  operators <- c(
    mustBe = "==",
    mustNotBe = "!=",
    mustBeGreaterThan = ">",
    mustBeGreaterOrEqualTo = ">=",
    mustBeLessThan = "<",
    mustBeLessOrEqualTo = "<="
  )
  selected <- intersect(names(rule), names(operators))
  if (
    length(selected) != 1L ||
      any(c("mustBeBetween", "mustNotBeBetween") %in% names(rule))
  ) {
    stop("Unsupported ODCS metric comparison.")
  }
  limit <- rule[[selected]]
  if (!is.numeric(limit) || length(limit) != 1L || !is.finite(limit)) {
    stop("ODCS metric comparison requires one finite numeric limit.")
  }
  unit <- rule$unit %||% "rows"
  if (metric == "rowCount" && unit != "rows") {
    stop("rowCount requires rows as its unit.")
  }
  if (!unit %in% c("rows", "percent")) {
    stop("Unsupported ODCS metric unit.")
  }
  if (length(setdiff(names(rule$arguments %||% list()), "column"))) {
    stop("Unsupported ODCS metric arguments.")
  }
  comparison <- get(operators[[selected]], envir = baseenv())
  check <- function(data) {
    data <- dataraft.core::dr_collect(data)
    value <- if (metric == "rowCount") {
      nrow(data)
    } else if (metric %in% c("nullValues", "missingValues")) {
      sum(is.na(data[[column]]))
    } else {
      sum(duplicated(data[[column]]))
    }
    if (unit == "percent") {
      value <- if (nrow(data)) 100 * value / nrow(data) else 0
    }
    comparison(value, limit)
  }
  result <- dataraft.core::dr_quality_rule(
    rule$name %||% paste(metric, column %||% "table"),
    check,
    action = if (identical(rule$severity, "warning")) "warn" else "block",
    dimension = rule$dimension
  )
  result$odcs <- rule
  result
}
