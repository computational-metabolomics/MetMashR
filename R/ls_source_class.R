#' @eval get_description('ls_source')
#' @include annotation_source_class.R
#' @family {annotation sources}
#' @family {annotation tables}
#' @export ls_source
ls_source = function(
        source,
        tag = 'LS',
        ...) {
    # new object
    out = new_struct(
        'ls_source',
        source = source,
        tag = tag,
        ...
    )
    return(out)
}


.ls_source<-setClass(
    "ls_source",
    contains = c('lcms_table'),
    prototype = list(
        data = .set_entity_value(
            obj = 'lcms_table',
            param_id = 'data',
            value = data.frame(
                id = character(0),
                mz = character(0),
                rt = character(0)
            )
        ),
        id_column = .set_entity_value(
            obj = 'lcms_table',
            param_id = 'id_column',
            value = 'id'
        ),
        mz_column = .set_entity_value(
            obj = 'lcms_table',
            param_id = 'mz_column',
            value = 'mz'
        ),
        rt_column = .set_entity_value(
            obj = 'lcms_table',
            param_id = 'rt_column',
            value = 'rt'
        )
    )
)

#' @export
setMethod(f = "read_source",
    signature = c("ls_source"),
    definition = function(obj) {
        M = obj
        # Locate from which row of LipidSearch output the data table starts
        con <- file(M$source, "r")
        lines <- readLines(con)
        line_num <- grep ("LipidIon", lines)
        close (con)
        if (length(line_num) < 1){
            stop ("Can't detect LipidSearch output in provided input_file!")
        }
        
        lipid_search_data <- read.csv(M$source, sep="\t",
            stringsAsFactors = F, skip=line_num-1)
        
        if (nrow(lipid_search_data)>0) {
            # get subtables 
            theor_mz <- lipid_search_data$CalcMz
            measured_mz_columns <- grep("ObsMz", colnames(lipid_search_data))
            measured_rt_columns <- grep("Rt.", colnames(lipid_search_data))
            grade_columns <- grep("Grade", colnames(lipid_search_data))
            
            out <- vector("list", nrow(lipid_search_data))
            
            for (lipid in 1:nrow(lipid_search_data)){
                # get column containing grade
                grade = lipid_search_data[lipid,grade_columns]
                grade_col=which(!is.na(grade) & nchar(grade)>0)
                
                # calc ppm and rt diff for library
                
                mz <- theor_mz[lipid]
                mz_meas <- lipid_search_data[lipid, measured_mz_columns][grade_col]
                
                ppm_diff = 1e6 * (mz_meas-mz)/mz
                
                rt = lipid_search_data[lipid, measured_rt_columns] * 60
                rt = rt[grade_col]
                
                lo=min(unlist(grade[grade_col]),na.rm = TRUE) # "best" grade
                w = which(grade[grade_col]==lo)[1]
                
                out[[lipid]] =
                    data.frame(
                        Rej. = lipid_search_data$Rej.[lipid],
                        LipidIon = lipid_search_data$LipidIon[lipid], 
                        LipidGroup = lipid_search_data$LipidGroup[lipid],
                        Class = lipid_search_data$Class[lipid], 
                        IonFormula = lipid_search_data$IonFormula[lipid],
                        theor_mass = mz,
                        Grade = grade[grade_col][[w]],
                        mz = mz_meas[[w]],
                        library_ppm_diff = ppm_diff[[w]],
                        rt = rt[[w]]
                    )
            }
            
            out <- do.call(rbind, out)
            
            # names
            N=strsplit(out$LipidIon,'[/+/-]',perl = TRUE)
            out$LipidName=unlist(lapply(N,'[',1))
            
            # add ids
            out$id = as.character(1:nrow(out))
            
        } else { # empty data.frame
            out= data.frame(matrix(nrow=0,ncol=(11+length(M$add_cols))))
            colnames(out)=c('Rej.','LipidIon','LipidGroup','Class',
                            'IonFormula','theor_mass','Grade','mz',
                            'library_ppm_diff','rt','LipidName')
            out$id=as.character(out$id)
        }
        
        M$data = out
        M$tag = M$tag

        return(M)
    }
)


