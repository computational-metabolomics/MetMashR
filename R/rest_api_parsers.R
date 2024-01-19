# internal function to parse json into a data.frame
.parse_pubchem_json = function(response,params){
    response = httr::content(response,as = 'text',encoding = 'UTF-8')
    J = jsonlite::fromJSON(response)
    J = as.data.frame(J)
    
    if (params$records=='best'){
        J=J[1,,drop=FALSE]
    }
    
    return(J)
}

# internal function to parse json property table
.parse_json_property = function(response,params){
    response = httr::content(response,as = 'text',encoding = 'UTF-8')
    J = jsonlite::fromJSON(response)
    J = as.data.frame(J$PropertyTable$Properties)
    
    if (params$records=='best'){
        J=J[1,,drop=FALSE]
    }
    
    return(J)
}

# internal function to parse json property table
.parse_hmdb_xml_ids = function(response,params){
    response = httr::content(response,as = 'text',encoding = 'UTF-8')
    
    J = XML::xmlTreeParse(response, asText=TRUE)
    J = XML::xmlToList(J)
    
    J = as.data.frame(J[params$output])
    return(J)
}

# internal function to parse lipidmaps json table
.parse_json_lipidmaps = function(response,params){
    response = httr::content(response,as = 'text',encoding = 'UTF-8')
    J = jsonlite::fromJSON(response)
    
    if (length(J)==0) {
        return(NA)
    }
    # single row
    if (names(J)[1]!='Row1') { 
        J=list(Row1=J)
    }
    # convert to data frames
    J = lapply(J,as.data.frame)
    # bind and fill with NA if missing
    J = plyr::rbind.fill(J)
    return(J)
}

# internal function to parse lipidmaps json table
.parse_json_classyfire = function(response,params){
    
    # get json and convert to list
    response = httr::content(response,as = 'text',encoding = 'UTF-8')
    J = jsonlite::fromJSON(response)
    
    # handle special case .all
    if (any(params$output_items == '.all')){
        params$output_items = c('kingdom','superclass','class','subclass',
                                'direct_parent', 'intermediate_nodes',
                                'substituents')
    }
    if (any(params$output_fields == '.all')) {
        params$output_fields = c('name', 'description', 'chemont_id','url')
    }
    
    # collapse vectors
    K=lapply(J[c('substituents','ancestors','predicted_chebi_terms')],
             function(x) {
                 paste0('| ',paste0(x,collapse=' | '),' |')
             }
    )
    J = modifyList(J,K)
    
    # filter to relevant columns
    J = J[params$output_items]
    
    # flatten
    J = unlist(J,recursive = FALSE)
    J = as.data.frame(J)
    
    # filter fields
    no_fields = c('substituents','ancestors','predicted_chebi_terms',
                  'smiles','description','molecular_framework')
    w = which(no_fields %in% params$output_items)
    if (length(w)>0) {
        no_fields = no_fields[w]
    }
    J = J %>% 
        select(contains(c(params$output_fields,no_fields))) %>%
        relocate(any_of(colnames(J))) # original column ordering
    
    return(J)
}

# internal function to parse mwb json into a data.frame
.parse_json_mwb = function(response,params){
    response = httr::content(response,as = 'text',encoding = 'UTF-8')
    J = jsonlite::fromJSON(response)
    J = as.data.frame(J)
    
    # drop search column
    J[params$input_item]=NULL
    return(J)
}

# internal function to parse mwb json into a data.frame
.parse_opsin = function(response,params){
    response = httr::content(response,as = 'text',encoding = 'UTF-8')
    J = data.frame(response = response)
    colnames(J)=params$output
    return(J)
}

