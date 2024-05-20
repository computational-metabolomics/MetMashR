#' @eval get_description('normalise_lipids')
#' @export
#' @include annotation_source_class.R
normalise_lipids = function(
        column_name,
        grammar = '.all',
        columns = '.all',
        suffix = '_goslin',
        batch_size = 10000,
        ...) {
    out = struct::new_struct(
        "normalise_lipids",
        column_name = column_name,
        grammar = grammar,
        columns = columns,
        suffix = suffix,
        batch_size = batch_size,
        ...
    )
    return(out)
}

.normalise_lipids = setClass(
    "normalise_lipids",
    contains = c("model"),
    slots = c(
        column_name = "entity",
        updated = "entity",
        grammar = "enum",
        columns = 'entity',
        suffix = 'entity',
        batch_size = 'entity'
    ),
    prototype = list(
        name = "Normalise Lipids nomenclature",
        description = paste0(
            "Normalises differently formated lipid names to ",
            "a consistent format."
        ),
        type = "univariate",
        predicted = "updated",
        libraries = "rgoslin",
        .params = c("column_name", "grammar","columns","suffix","batch_size"),
        .outputs = c("updated"),
        column_name = entity(
            name = "Lipid column name",
            description = paste0(
                "The name of the column containing Lipids ",
                "names to normalise."
            ),
            type = c("character"),
            value = "V1",
            max_length = 1
        ),
        updated = entity(
            name = "Updated annotations",
            description = "annotation_source after normalising lipid names",
            type = "annotation_source",
            max_length = Inf
        ),
        columns = entity(
            name = "Output columns",
            description = paste0(
                'Column names to include from the goslin output. Can ',
                'be any of "Normalized.Name", "Original.Name", "Grammar", ',                  
                '"Adduct", "Adduct.Charge", "Lipid.Maps.Category", ',
                '"Lipid.Maps.Main.Class", "Species.Name", "Extended.Species.Name", ',
                '"Molecular.Species.Name", "Sn.Position.Name", ',
                '"Structure.Defined.Name", "Full.Structure.Name", ',
                '"Functional.Class.Abbr", "Functional.Class.Synonyms", "Level", ',
                '"Total.C", "Total.OH", "Total.O", "Total.DB", "Mass", "Sum.Formula".',
                '".all" will return all columns. "'
            ),
            type = "character",
            max_length = Inf,
            value = '.all'
        ),
        suffix = entity(
            name = "Column name suffix",
            description = "A suffic added to the column names of the goslin output.",
            type = "character",
            max_length = 1,
            value = '_goslin'
        ),
        grammar = enum(
            name = "Grammar",
            description = paste0(
                "The grammar to use for normalising lipid ",
                "names. Allowed values are: Shorthand2020, Goslin, FattyAcids,",
                " LipidMaps, SwissLipids, HMDB or .all"
            ),
            type = "character",
            allowed = c(
                "Shorthand2020",
                "Goslin",
                "FattyAcids",
                "LipidMaps",
                "SwissLipids",
                "HMDB",
                ".all"
            ),
            value = ".all"
        ),
        batch_size = entity(
            name = 'Batch size',
            description = paste0(
                'The maximum number of annotations to be parsed by rgoslin at ',
                'a time. If the batch size is less than the total number of ',
                'records then the records will be split into multiple batches ',
                'to help prevent crashes.'
            ),
            value = 10000,
            type = c('numeric','integer'),
            max_length = 1
        )
    )
)


#' @export
setMethod(
    f = "model_apply",
    signature = c("normalise_lipids", "annotation_source"),
    definition = function(M, D) {
        X = D$data
        
        if (M$grammar == ".all") {
            grammar = NULL
        } else {
            grammar = M$grammar
        }
        
        batch = unique(
            c(seq(from=1,to=nrow(D$data),by=M$batch_size),nrow(D$data)+1)
        )
         
        G = NULL
        for (k in seq_len(length(batch)-1)) {
            print(k)
            start = batch[k]
              end = batch[k+1]-1
            
            L = suppressMessages(
                lapply(
                    X[[M$column_name]][start:end],
                    rgoslin::parseLipidNames,
                    grammar = grammar
                ))
            
            L = plyr::rbind.fill(L)
            G = plyr::rbind.fill(G,L)
        }
        
        # update lipidmaps categories
        B = BiocFileCache_database(
            source = 'https://raw.githubusercontent.com/lifs-tools/goslin/master/lipid-list.csv',
            import_fun = read.csv,
            resource_name = 'goslin-lipidmaps'
        )
        B = read_database(B)
        B = B %>% select(all_of(c('Lipid.name','Lipid.category','Lipid.description')))
        
        for (r in seq_len(nrow(G))){
            
            # description
            w = which(B$Lipid.category==G[['Lipid.Maps.Category']][r] & B$Lipid.name==G[["Lipid.Maps.Main.Class"]][r])
            if (length(w)>0){
                G[["Lipid.Maps.Main.Class"]][r]=B[['Lipid.description']][w[1]]
            }
            
            # category
            w = which(names(.lipid_maps_classes)==G[['Lipid.Maps.Category']][r])
            if (length(w)>0) {
                G[['Lipid.Maps.Category']][r] = .lipid_maps_classes[w]
            }

        }

        cols = M$columns
        if (any(cols == '.all')) {
            cols = colnames(G)
        }
        
        X = cbind(X, G[,cols,drop=FALSE])
        
        D$data = X
        M$updated = D
        
        return(M)
    }
)


.lipid_maps_classes = c(
    FA = 'Fatty Acyls [FA]',
    GL = 'Glycerolipids [GL]',
    GP = 'Glycerophospholipids [GP]',
    SP = 'Sphingolipids [SP]',
    ST = 'Sterol Lipids [ST]',
    PR = 'Prenol Lipids [PR]',
    SL = 'Saccarolipids [SL]',
    PK = 'Polyketides [PK]'
)
