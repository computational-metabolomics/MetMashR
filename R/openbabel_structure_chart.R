#' @eval get_description('openbabel_structure')
#' @import ggplot2
#' @rawNamespace import(cowplot, except = theme_map)
#' @export
openbabel_structure <- function(smiles_column = "smiles",
    row_index = 1,
    image_size = 300,
    hydrogens = "implicit",
    carbons = "terminal",
    double_bonds = "asymmetric",
    colour_atoms = TRUE,
    scale_to_fit = TRUE,
    view_port = 300,
    title_column = NULL,
    subtitle_column = NULL,
    ...) {
    out <- struct::new_struct(
        "openbabel_structure",
        smiles_column = smiles_column,
        row_index = row_index,
        image_size = image_size,
        hydrogens = hydrogens,
        carbons = carbons,
        double_bonds = double_bonds,
        colour_atoms = colour_atoms,
        scale_to_fit = scale_to_fit,
        view_port = view_port,
        title_column = title_column,
        subtitle_column = subtitle_column,
        ...
    )

    return(out)
}


.openbabel_structure <- setClass(
    "openbabel_structure",
    contains = "chart",
    slots = c(
        smiles_column = "entity",
        row_index = "entity",
        image_size = "entity",
        hydrogens = "enum",
        scale_to_fit = "entity",
        carbons = "enum",
        double_bonds = "enum",
        colour_atoms = "entity",
        view_port = "entity",
        title_column = "entity",
        subtitle_column = "entity"
    ),
    prototype = list(
        name = "OpenBabel molecular structure",
        description = paste0(
            "Display an image of the molecular structure computed using ",
            "OpenBabel."
        ),
        type = "image",
        .params = c(
            "smiles_column", "image_size", "hydrogens", "carbons",
            "double_bonds",
            "colour_atoms", "scale_to_fit", "row_index", "view_port",
            "title_column", "subtitle_column"
        ),
        libraries = c("ChemmineOB", "cowplot", "rsvg"),
        smiles_column = entity(
            name = "PubChem query column",
            value = "V1",
            type = c("character"),
            description = paste0(
                "The name of the `annotation_source` column with compound ",
                "identifiers of the type specified in the `search_by` param."
            )
        ),
        row_index = entity(
            name = "Row index",
            description = paste0(
                "The row index of the `annotation_source` to request an image ",
                "of the molecular structure of."
            ),
            type = c("integer", "numeric"),
            value = 1
        ),
        image_size = entity(
            name = "Image size",
            description = paste0(
                "The size of the image to return in pixels. Images will be ",
                "square."
            ),
            type = c("numeric", "integer"),
            value = 300,
            max_length = 1
        ),
        hydrogens = enum(
            name = "Hydrogen atoms",
            description = c(
                "implicit" = "Hydrogen atoms are not displayed.",
                "explicit" = "All hydrogen atoms are displayed."
            ),
            allowed = c("implicit", "explicit"),
            value = "implicit",
            type = "character",
            max_length = 1
        ),
        scale_to_fit = entity(
            name = "Normalise coordinates",
            description = c(
                "TRUE" = paste0(
                    "Molecules will be scaled to fit inside the bounding box ",
                    "of the image."
                ),
                "FALSE" = paste0(
                    "Molecules will not be scaled to fit inside the ",
                    "bounding box of the image."
                )
            ),
            value = TRUE,
            type = "logical",
            max_length = 1
        ),
        colour_atoms = entity(
            name = "Coloured atoms",
            description = paste0(
                "Display some atoms in colour."
            ),
            value = TRUE,
            type = "logical",
            max_length = 1
        ),
        carbons = enum(
            name = "Carbon atoms",
            description = c(
                "none" = "Carbon atoms are not labelled.",
                "terminal" = "Terminal carbons and hydrogens are labelled.",
                "all" = "All carbon atoms will be labelled."
            ),
            allowed = c("none", "terminal", "all"),
            value = "terminal",
            type = "character",
            max_length = 1
        ),
        double_bonds = enum(
            name = "Double bonds",
            description = paste0(
                "The display style of double carbon bonds."
            ),
            allowed = c("symmetric", "asymmetric"),
            value = "symmetric",
            type = "character",
            max_length = 1
        ),
        view_port = entity(
            name = "View port",
            description = paste0(
                "Scales the image insde the viewport. Can be used to ensure a ",
                "set of images have the same bond lengths and font sizes. ",
                "Has no effect if `scale_to_fit = TRUE`. The molecule might ",
                "be clipped if the viewport is too small."
            ),
            type = c("numeric", "integer"),
            value = 300,
            max_length = 1
        ),
        title_column = entity(
            name = "Title column",
            description = paste0(
                "The column containing text to use as the title for the ",
                "image. If NULL then no title is included."
            ),
            type = c("NULL", "character"),
            value = NULL,
            max_length = 1
        ),
        subtitle_column = entity(
            name = "Title column",
            description = paste0(
                "The column containing text to use as the subtitle for the ",
                "image. If NULL then no subtitle is included."
            ),
            type = c("NULL", "character"),
            value = NULL,
            max_length = 1
        )
    )
)

#' @export
#' @template chart_plot
setMethod(
    f = "chart_plot",
    signature = c("openbabel_structure", "character"),
    definition = function(obj, dobj) {
        # create annotation_source
        A <- annotation_source(
            annotations = data.frame(id = 1, smiles = dobj)
        )

        # ensure compatability with new table
        obj$row_index <- 1
        obj$smiles_column <- "smiles"

        # plot
        g <- chart_plot(obj, A)

        return(g)
    }
)


#' @export
#' @template chart_plot
setMethod(
    f = "chart_plot",
    signature = c("openbabel_structure", "annotation_source"),
    definition = function(obj, dobj) {
        # get smiles
        smiles <- dobj$data[obj$row_index, obj$smiles_column]

        # get titles
        title <- element_blank()
        subtitle <- element_blank()

        if (!is.null(obj$title_column)) {
            title <- dobj$data[obj$row_index, obj$title_column]
        }
        if (!is.null(obj$subtitle_column)) {
            subtitle <- dobj$data[obj$row_index, obj$subtitle_column]
        }

        # create image options
        genOpts <- data.frame(name = "title", value = "")

        if (obj$hydrogens == "implicit") {
            genOpts <- rbind(
                genOpts,
                data.frame(name = "d", value = "")
            )
        }
        if (obj$hydrogens == "explicit") {
            genOpts <- rbind(
                genOpts,
                data.frame(name = "h", value = "")
            )
        }

        outOpts <- data.frame(
            name = c("P", "d"), value = c(obj$image_size, "d")
        )

        if (obj$carbons == "none") {
            outOpts <- rbind(
                outOpts,
                data.frame(name = "C", value = "")
            )
        }
        if (obj$carbons == "all") {
            outOpts <- rbind(
                outOpts,
                data.frame(name = "a", value = "")
            )
        }

        if (obj$double_bonds == "asymmetric") {
            outOpts <- rbind(
                outOpts,
                data.frame(name = "s", value = "")
            )
        }

        if (!obj$colour_atoms) {
            outOpts <- rbind(
                outOpts,
                data.frame(name = "u", value = "")
            )
        }

        # temporary file
        tf <- tempfile(fileext = ".svg")

        # create image
        ChemmineOB::convertToImage(
            "smiles", "svg", smiles, tf,
            genOpts, outOpts
        )

        # rescale
        if (!obj$scale_to_fit) {
            # read svg
            r <- readLines(tf)

            # get image size
            vb <- regmatches(r[8], regexpr('\"([^\"]*)\"$', r[8]))
            vb <- gsub('\"', "", vb)
            lims <- as.numeric(strsplit(vb, " ")[[1]])

            # new view port
            newvp <- c(0, 0, obj$view_port, obj$view_port)
            midx <- lims[3] / 2
            midy <- lims[4] / 2
            # move to center
            offx <- midx - (newvp[3] / 2)
            offy <- midy - (newvp[4] / 2)

            # make replacement
            str <- paste0(
                '\"', offx, " ", offy, " ",
                newvp[3], " ", newvp[4], '\"'
            )

            # replace
            r[8] <- sub('\"([^\"]*)\"$', str, r[8])

            # write to file
            writeLines(r, tf)
        }

        # read image
        img <- rsvg::rsvg(tf, width = obj$image_size, height = obj$image_size)

        # delete temp file
        file.remove(tf)

        # plot
        h <- ggplot() +
            ggtitle(title, subtitle) +
            theme_void()
        g <- cowplot::ggdraw() +
            cowplot::draw_image(img, scale = 0.95) +
            cowplot::draw_plot(h)


        return(g)
    }
)
