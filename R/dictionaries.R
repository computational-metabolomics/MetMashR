#' Tripeptide dictionary
#'
#' A dictionary for converting tripeptides encoded using single letter
#' IUPAC codes to use three letter codes for amino acids separated by hyphens.
#' e.g. INK becomes Ile-Asn-Lys
#'
#' @examples
#' M <- normalise_strings(
#'     search_column = "example",
#'     output_column = "result",
#'     dictionary = .tripeptide_dictionary
#' )
#'
#' @return A dictionary for use with [`normalise_strings()`]
#' @export
.tripeptide_dictionary <- list(
    list(
        pattern = "^[ABCDEFGHIJKLMNOPQRSTVWXYZ]{3}$",
        replace = function(x) {
            aa <- list(
                "A" = "Ala",
                "B" = "Asx",
                "C" = "Cys",
                "D" = "Asp",
                "E" = "Glu",
                "F" = "Phe",
                "G" = "Gly",
                "H" = "His",
                "I" = "Ile",
                "J" = "Xle",
                "K" = "Lys",
                "L" = "Leu",
                "M" = "Met",
                "N" = "Asn",
                "O" = "Pyl",
                "P" = "Pro",
                "Q" = "Gln",
                "R" = "Arg",
                "S" = "Ser",
                "T" = "Thr",
                "U" = "Sec",
                "V" = "Val",
                "W" = "Trp",
                "X" = "Unk",
                "Y" = "Tyr",
                "Z" = "Glx"
            )

            # split to chars
            x <- unlist(strsplit(x$x, ""))

            x <- lapply(x, function(y) {
                return(aa[[y]])
            })

            x <- unlist(x)
            x <- paste0(x, collapse = "-")

            return(x)
        }
    )
)


#' Racemic dictionary
#'
#' This dictionary removes racemic properties from molecule names. It is
#' intended for use with the [`normalise_strings()`] object.
#'
#' @examples
#' M <- normalise_strings(
#'     search_column = "example",
#'     output_column = "result",
#'     dictionary = .racemic_dictionary
#' )
#'
#' @return A dictionary for use with [`normalise_strings()`]
#' @export
.racemic_dictionary <- list(
    list(pattern = "-(-)-", replace = "-", fixed = TRUE),
    list(pattern = "-(+)-", replace = "-", fixed = TRUE),
    list(pattern = "^L(+)-", replace = "L-", fixed = TRUE),
    list(pattern = "^D(-)-", replace = "D-", fixed = TRUE),
    list(pattern = "(\\x{00b1})", replace = "", fixed = TRUE)
)


#' Greek dictionary
#'
#' A dictionary for converting Greek characters to Romanised names. It is
#' intended for use with the [`normalise_strings()`] object.
#'
#' @examples
#' M <- normalise_strings(
#'     search_column = "example",
#'     output_column = "result",
#'     dictionary = .greek_dictionary
#' )
#'
#' @return A dictionary for use with [`normalise_strings()`]
#' @export
.greek_dictionary <- list(
    list(pattern = "\\x{03b1}", replace = "alpha"),
    list(pattern = "\\x{03b2}", replace = "beta"),
    list(pattern = "\\x{03b3}", replace = "gamma"),
    list(pattern = "\\x{03b4}", replace = "delta"),
    list(pattern = "\\x{03b5}", replace = "epsilon"),
    list(pattern = "\\x{03b6}", replace = "zeta"),
    list(pattern = "\\x{03b7}", replace = "eta"),
    list(pattern = "\\x{03b8}", replace = "theta"),
    list(pattern = "\\x{03b9}", replace = "iota"),
    list(pattern = "\\x{03ba}", replace = "kappa"),
    list(pattern = "\\x{03bb}", replace = "lambda"),
    list(pattern = "\\x{03bc}", replace = "mu"),
    list(pattern = "\\x{03bd}", replace = "nu"),
    list(pattern = "\\x{03be}", replace = "xi"),
    list(pattern = "\\x{03bf}", replace = "omicron"),
    list(pattern = "\\x{03c0}", replace = "pi"),
    list(pattern = "\\x{03c1}", replace = "rho"),
    list(pattern = "\\x{03c3}", replace = "sigma"),
    list(pattern = "\\x{03c4}", replace = "tau"),
    list(pattern = "\\x{03c5}", replace = "upsilon"),
    list(pattern = "\\x{03c6}", replace = "phi"),
    list(pattern = "\\x{03c7}", replace = "chi"),
    list(pattern = "\\x{03c8}", replace = "psi"),
    list(pattern = "\\x{03c9}", replace = "omega"),
    list(pattern = "\\x{0391}", replace = "alpha"),
    list(pattern = "\\x{0392}", replace = "beta"),
    list(pattern = "\\x{0393}", replace = "gamma"),
    list(pattern = "\\x{0394}", replace = "delta"),
    list(pattern = "\\x{0395}", replace = "epsilon"),
    list(pattern = "\\x{0396}", replace = "zeta"),
    list(pattern = "\\x{0397}", replace = "eta"),
    list(pattern = "\\x{0398}", replace = "theta"),
    list(pattern = "\\x{0399}", replace = "iota"),
    list(pattern = "\\x{039a}", replace = "kappa"),
    list(pattern = "\\x{039b}", replace = "lambda"),
    list(pattern = "\\x{039c}", replace = "mu"),
    list(pattern = "\\x{039d}", replace = "nu"),
    list(pattern = "\\x{039e}", replace = "xi"),
    list(pattern = "\\x{039f}", replace = "omicron"),
    list(pattern = "\\x{03a0}", replace = "pi"),
    list(pattern = "\\x{03a1}", replace = "rho"),
    list(pattern = "\\x{03a3}", replace = "sigma"),
    list(pattern = "\\x{03a4}", replace = "tau"),
    list(pattern = "\\x{03a5}", replace = "upsilon"),
    list(pattern = "\\x{03a6}", replace = "phi"),
    list(pattern = "\\x{03a7}", replace = "chi"),
    list(pattern = "\\x{03a8}", replace = "psi"),
    list(pattern = "\\x{03a9}", replace = "omega")
)

# K(?<=^[A-Z]{3})$
# K(?<=^[A-Z]{2})(?=[A-Z]{1}$)
# ^K(?=[A-Z]{2}$)
