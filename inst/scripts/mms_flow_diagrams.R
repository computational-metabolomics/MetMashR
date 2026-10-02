# Flow diagrams for the MMS case study vignette (vignettes/mms_case_study.Rmd).
#
# Builds the diagram for each MMS step from the vignette's own model sequences
# with structDAG (as_dag -> dag_chart -> chart_plot), then routes the edges so
# that every line is exactly horizontal or vertical and meets the boxes
# exactly. Steps that use the same function in both variants are drawn once;
# where the variants differ the path splits into one row per variant, with
# edges labelled by workflow.
#
# The vignette includes the saved PNG files, so these packages are only needed
# to regenerate the figures (they are not MetMashR dependencies): pkgload,
# knitr, structDAG, DiagrammeR, DiagrammeRsvg (which needs V8) and rsvg.
#
# Run from the MetMashR source directory:
#   Rscript inst/scripts/mms_flow_diagrams.R [output directory]
# The output directory defaults to vignettes/mms_figures.
args <- commandArgs(TRUE)
out_dir <- if (length(args) > 0) args[1] else file.path("vignettes", "mms_figures")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

pkgload::load_all(".", quiet = TRUE)
suppressMessages({library(struct); library(structDAG)})

# step definitions from the vignette (no lookups are run)
code <- knitr::purl(file.path("vignettes", "mms_case_study.Rmd"),
                    output = tempfile(fileext = ".R"), quiet = TRUE)
lines <- readLines(code)
lines <- lines[grep("^## ----cache-setup", lines):(grep("^## ----full-workflow", lines) - 1)]
eval(parse(text = grep("include_graphics", lines, invert = TRUE, value = TRUE)), envir = globalenv())

variants <- c("RefMet", "ChEBI")

# step kind (colour) and a description taken from each model's parameters
step_kind <- function(m) {
    cls <- class(m)[1]
    if (grepl("lookup|exchange", cls)) return("lookup")
    switch(cls,
        filter_na = , unique_records = "filter",
        combine_records = "merge",
        split_records = "split",
        "compute")
}
step_detail <- function(m) {
    switch(class(m)[1],
        pubchem_id_exchange = "PubChem: name to InChIKey",
        mwb_refmet_lookup = "RefMet: name to InChIKey",
        chebi_lookup = "ChEBI: name to InChIKey",
        pubchem_property_lookup = "PubChem: properties",
        classyfire_batch_lookup = "ClassyFire: chemical classes",
        mwb_compound_lookup = "MWB: cross-references",
        kegg_lookup = "KEGG: ChEBI to KEGG id",
        cts_lite_lookup = "CTS-Lite: names and counts",
        compute_column = m$output_column,
        prioritise_columns = paste(m$source_tags, collapse = " > "),
        filter_na = paste("drop missing", m$column_name),
        combine_records = paste("merge by", m$group_by),
        unique_records = "drop duplicates",
        normalise_strings = m$output_column,
        split_records = paste("split", m$column_name),
        "")
}
label_of <- function(m) paste0(class(m)[1], "()\\n", step_detail(m))

# two steps are drawn as one if they use the same function, even if their
# settings (e.g. input column, cache) differ, to keep the diagrams simple
same_step <- function(a, b) identical(class(a), class(b))

# label for a step shared by both variants; where the descriptions differ,
# both are shown
shared_label <- function(a, b) {
    da <- step_detail(a); db <- step_detail(b)
    if (identical(da, db)) return(label_of(a))
    if (is(a, "prioritise_columns")) {
        ta <- a$source_tags; tb <- b$source_tags
        detail <- paste(ifelse(ta == tb, ta, paste(ta, "or", tb)), collapse = " > ")
    } else {
        detail <- paste(da, "or", db)
    }
    paste0(class(a)[1], "()\\n", detail)
}

# longest run of matching steps (in order) between two sequences
match_steps <- function(A, B) {
    n <- length(A); m <- length(B)
    S <- outer(seq_len(n), seq_len(m), Vectorize(function(i, j) same_step(A[[i]], B[[j]])))
    L <- matrix(0, n + 1, m + 1)
    for (i in n:1) for (j in m:1) {
        L[i, j] <- if (S[i, j]) L[i + 1, j + 1] + 1 else max(L[i + 1, j], L[i, j + 1])
    }
    pairs <- list(); i <- 1; j <- 1
    while (i <= n && j <= m) {
        if (S[i, j]) {
            pairs[[length(pairs) + 1]] <- c(i, j); i <- i + 1; j <- j + 1
        } else if (L[i + 1, j] >= L[i, j + 1]) {
            i <- i + 1
        } else {
            j <- j + 1
        }
    }
    pairs
}

# Combine the two variants' sequences into one graph: matching steps are
# drawn once on the centre row, divergent steps on one row per variant
# (RefMet above, ChEBI below), with edges on divergent paths labelled by
# workflow
build_graph <- function(A, B) {
    nA <- as_dag(Reduce(`+`, A))$nodes
    nB <- as_dag(Reduce(`+`, B))$nodes
    pairs <- match_steps(A, B)
    split <- length(pairs) < max(length(A), length(B))
    row_of <- if (split) c(a = 0, shared = 0.5, b = 1) else c(a = 0, shared = 0, b = 0)

    nodes <- list(); pos <- character(0); shared <- logical(0)
    E <- data.frame(from = character(0), to = character(0), label = character(0))
    col <- 0
    add_node <- function(n, m, id, row, is_shared, label = label_of(m)) {
        n$id <- id
        n$name <- label
        n$type <- step_kind(m)
        nodes[[id]] <<- n
        shared[id] <<- is_shared
        pos[id] <<- sprintf("%g,%d", row, col)
    }
    add_edge <- function(from, to, label) {
        if (!is.null(from)) E[nrow(E) + 1, ] <<- c(from, to, label)
    }
    prevA <- prevB <- NULL
    ia <- ib <- 1
    for (p in c(pairs, list(c(length(A) + 1, length(B) + 1)))) {
        # divergent steps before the next matching step (or the end)
        segA <- seq_len(p[1] - ia) + ia - 1
        segB <- seq_len(p[2] - ib) + ib - 1
        start <- col
        # trailing steps that only one variant has stay on the centre row
        last <- p[1] > length(A)
        rowA <- if (last && !length(segB)) row_of["shared"] else row_of["a"]
        rowB <- if (last && !length(segA)) row_of["shared"] else row_of["b"]
        for (k in segA) {
            col <- start + (k - ia)
            id <- paste0("a", k)
            add_node(nA[[k]], A[[k]], id, rowA, FALSE)
            add_edge(prevA, id, variants[1]); prevA <- id
        }
        for (k in segB) {
            col <- start + (k - ib)
            id <- paste0("b", k)
            add_node(nB[[k]], B[[k]], id, rowB, FALSE)
            add_edge(prevB, id, variants[2]); prevB <- id
        }
        col <- start + max(length(segA), length(segB))
        if (p[1] > length(A)) break
        # matching step, drawn once
        id <- paste0("s", p[1])
        add_node(nA[[p[1]]], A[[p[1]]], id, row_of["shared"], TRUE,
                 label = shared_label(A[[p[1]]], B[[p[2]]]))
        if (identical(prevA, prevB)) {
            add_edge(prevA, id, "")
        } else {
            add_edge(prevA, id, variants[1])
            add_edge(prevB, id, variants[2])
        }
        prevA <- prevB <- id
        col <- col + 1
        ia <- p[1] + 1; ib <- p[2] + 1
    }
    edges <- lapply(seq_len(nrow(E)), function(i) {
        edge(from = E$from[i], to = E$to[i], from_param = "predicted", to_param = "D")
    })
    list(nodes = nodes, edges = edges, edge_info = E, shared = shared, pos = pos)
}

legend_graph <- function() {
    k <- c(lookup = "external lookup", compute = "compute column",
           filter = "filter records", merge = "merge records",
           split = "split records")
    nodes <- list(); pos <- character(0)
    for (i in seq_along(k)) {
        id <- paste0("legend_", i)
        nodes[[id]] <- node(id = id, name = k[[i]], type = names(k)[i], fcn = function(D) list())
        pos[id] <- sprintf("0,%d", i - 1)
    }
    list(nodes = nodes, edges = list(), pos = pos,
         edge_info = data.frame(from = character(0), to = character(0), label = character(0)),
         shared = setNames(rep(TRUE, length(k)), names(nodes)))
}

num <- function(x) as.numeric(regmatches(x, gregexpr("-?[0-9]+\\.?[0-9]*", x))[[1]])
f4 <- function(x) sprintf("%.4f", x)

# Replace Graphviz's edges with exact ones: straight lines between boxes in
# a row, and horizontal-vertical-horizontal connectors where a path splits
# from or rejoins a shared step. Each line starts on the source box's right
# edge, and its arrow tip lands on the target box's left edge at the box's
# vertical centre. Edges on divergent paths get a workflow label.
route_edges <- function(svg, g) {
    ids <- names(g$nodes) # graphviz numbers nodes in this order
    node_re <- '(?s)<g id="node[0-9]+" class="node">\\s*<title>([^<]*)</title>(.*?)</g>'
    box <- list()
    for (n in regmatches(svg, gregexpr(node_re, svg, perl = TRUE))[[1]]) {
        v <- num(regmatches(n, regexpr(' (points|d)="[^"]+"', n)))
        x <- v[seq(1, length(v), by = 2)]; y <- v[seq(2, length(v), by = 2)]
        box[[ids[as.integer(sub(node_re, "\\1", n, perl = TRUE))]]] <-
            c(x0 = min(x), x1 = max(x), y = (min(y) + max(y)) / 2)
    }
    edge_re <- '(?s)<g id="edge[0-9]+" class="edge">\\s*<title>([^<]*)</title>.*?</g>'
    stub <- 8; arrow_len <- 5; arrow_half <- 1.75; label_size <- 7
    for (e in regmatches(svg, gregexpr(edge_re, svg, perl = TRUE))[[1]]) {
        ft <- as.integer(strsplit(gsub("&#45;&gt;", "->", sub(edge_re, "\\1", e, perl = TRUE)),
                                  "->", fixed = TRUE)[[1]])
        from <- ids[ft[1]]; to <- ids[ft[2]]
        info <- g$edge_info[g$edge_info$from == from & g$edge_info$to == to, ]
        s <- box[[from]]; t <- box[[to]]
        xs <- s[["x1"]]; ys <- s[["y"]]; xt <- t[["x0"]]; yt <- t[["y"]]
        xb <- xt - arrow_len
        if (ys == yt) {
            pts <- rbind(c(xs, ys), c(xb, yt))
            lab <- c((xs + xt) / 2, ys)
        } else if (g$shared[[from]]) {
            xv <- xs + stub # split: turn just after the shared step
            pts <- rbind(c(xs, ys), c(xv, ys), c(xv, yt), c(xb, yt))
            lab <- c((xv + xt) / 2, yt)
        } else {
            xv <- xt - stub # rejoin: turn just before the shared step
            pts <- rbind(c(xs, ys), c(xv, ys), c(xv, yt), c(xb, yt))
            lab <- c((xs + xv) / 2, ys)
        }
        d <- paste0("M", paste(f4(pts[, 1]), f4(pts[, 2]), sep = ",", collapse = "L"))
        arrow <- sprintf("%s,%s %s,%s %s,%s %s,%s",
            f4(xb), f4(yt - arrow_half), f4(xt), f4(yt), f4(xb), f4(yt + arrow_half),
            f4(xb), f4(yt - arrow_half))
        text <- ""
        if (nrow(info) == 1 && nzchar(info$label)) {
            text <- sprintf(paste0('\n<text text-anchor="middle" x="%s" y="%s" ',
                'font-family="Helvetica,sans-Serif" font-size="%.2f" fill="#333333">%s</text>'),
                f4(lab[1]), f4(lab[2] - 3), label_size, info$label)
        }
        e2 <- sub(' d="[^"]+"', paste0(' d="', d, '"'), e)
        e2 <- sub(' points="[^"]+"', paste0(' points="', arrow, '"'), e2)
        e2 <- sub("</g>$", paste0(text, "\n</g>"), e2)
        svg <- sub(e, e2, svg, fixed = TRUE)
    }
    svg
}

palette <- list(lookup = "#9ABDDC", compute = "#f7d7bf",
                filter = "#e76e50", merge = "#e8c468", split = "#bcf3a9")

render_graph <- function(g) {
    D <- dag(nodes = g$nodes, edges = g$edges)
    layout <- paste(sprintf("%d: %s", seq_along(g$pos), g$pos[names(g$nodes)]),
                    collapse = " | ")
    DC <- dag_chart(
        node_colour = palette,
        node_shape = "box",
        node_border_colour = "#333333",
        edge_colour = "#333333",
        node_font_size = 9,
        node_width = 1.9,
        node_height = 0.6,
        layout = "custom",
        custom_layout = layout,
        custom_grid_width = 2.5,
        custom_grid_height = 1
    )
    route_edges(DiagrammeRsvg::export_svg(chart_plot(DC, D)), g)
}

# widen an svg to `width` pt, centring the diagram, so that every figure
# shares one scale when shown at the full text width
pad_svg <- function(svg, width) {
    vb <- num(regmatches(svg, regexpr('viewBox="[^"]+"', svg)))
    dx <- (width - vb[3]) / 2
    svg <- sub('<svg width="[^"]+"', sprintf('<svg width="%gpt"', width), svg)
    svg <- sub('viewBox="[^"]+"', sprintf('viewBox="0.00 0.00 %g %g"', width, vb[4]), svg)
    tr <- regmatches(svg, regexpr("translate\\([^)]+\\)", svg))
    t0 <- num(tr)
    svg <- sub(tr, sprintf("translate(%g %g)", t0[1] + dx, t0[2]), svg, fixed = TRUE)
    # extend the white background polygon to the new width
    bg_re <- '<polygon fill="#ffffff" stroke="transparent" points="[^"]+"'
    bg <- regmatches(svg, regexpr(bg_re, svg))
    y <- num(sub('.*points="([^"]+)"', "\\1", bg))[c(2, 4)]
    x0 <- -t0[1] - dx; x1 <- width - t0[1] - dx
    pts <- sprintf("%g,%g %g,%g %g,%g %g,%g %g,%g", x0, y[1], x0, y[2], x1, y[2], x1, y[1], x0, y[1])
    sub(bg_re, sprintf('<polygon fill="#ffffff" stroke="transparent" points="%s"', pts), svg)
}

figures <- list(
    mms_legend = render_graph(legend_graph()),
    mms_step1_translation = render_graph(build_graph(translate_refmet, translate_chebi)),
    mms_step2_attributes = render_graph(build_graph(attribute_step, attribute_step_chebi)),
    mms_step3_curation = render_graph(build_graph(curation_step, curation_step)),
    mms_normalise = render_graph(build_graph(normalise_step, normalise_step))
)

width <- max(vapply(figures, function(svg) num(regmatches(svg, regexpr('viewBox="[^"]+"', svg)))[3], 1))
for (nm in names(figures)) {
    svg <- pad_svg(figures[[nm]], width)
    rsvg::rsvg_png(charToRaw(svg), file.path(out_dir, paste0(nm, ".png")), width = 1800)
}
cat("done; common width", width, "pt\n")
