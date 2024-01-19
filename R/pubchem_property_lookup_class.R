#' @eval get_description('pubchem_property_lookup')
#' @export
#' @include annotation_source_class.R rest_api_class.R
pubchem_property_lookup = function(
        query_column,
        search_by,
        suffix = '_pubchem',
        property = 'InChIKey',
        ...) {
    
    
    
    # check properties
    
    allowed = c(
        'MolecularFormula',
        'MolecularWeight',	
        'CanonicalSMILES',	
        'IsomericSMILES',
        'InChI',
        'InChIKey',
        'IUPACName',
        'Title',
        'XLogP',
        'ExactMass',
        'MonoisotopicMass',
        'TPSA',
        'Complexity',
        'Charge',
        'HBondDonorCount',
        'HBondAcceptorCount',
        'RotatableBondCount',
        'HeavyAtomCount',
        'IsotopeAtomCount',
        'AtomStereoCount',
        'DefinedAtomStereoCount',
        'UndefinedAtomStereoCount',
        'BondStereoCount',
        'DefinedBondStereoCount',
        'UndefinedBondStereoCount',
        'CovalentUnitCount',
        'PatentCount',
        'PatentFamilyCount',
        'LiteratureCount',
        'Volume3D',
        'XStericQuadrupole3D',
        'YStericQuadrupole3D',
        'ZStericQuadrupole3D',
        'FeatureCount3D',
        'FeatureAcceptorCount3D',
        'FeatureDonorCount3D',
        'FeatureAnionCount3D',
        'FeatureCationCount3D',
        'FeatureRingCount3D',
        'FeatureHydrophobeCount3D',
        'ConformerModelRMSD3D',
        'EffectiveRotorCount3D',
        'ConformerCount3D',
        'Fingerprint2D',
        '.all'
    )
    
    # check valid property
    check = all(property %in% allowed)
    if (!check){
        w=which(!(property %in% allowed))
        stop("Invalid properties: ", paste0(property[w],collapse=','))
    }
    
    # check for all
    if (any(property == '.all')) {
        property = allowed
    }
    
    # remove .all
    w=which(property=='.all')
    if (length(w)>0) {
        property=property[-w]
    }
    
    # if vector, collapse to comma separated
    if (length(property)>1) {
        property = paste0(property,collapse=',')
    }
    
    out=struct::new_struct('pubchem_property_lookup',
                           query_column = query_column,
                           search_by = search_by,
                           suffix = suffix,
                           property = property,
                           ...)
    
    return(out)
}


.pubchem_property_lookup<-setClass(
    "pubchem_property_lookup",
    contains = c('pubchem_compound_lookup'),
    slots=c(
        property='entity'
    ),
    
    prototype=list(
        name = 'Compound property lookup via pubchem',
        description = paste0('Uses the PubChem API to search for CID based on 
            the input annotation column and returns property information.'),
        type = 'rest_api',
        predicted = 'updated',
        .params=c('search_by','property'),
        
        url_template = entity(
            name = 'URL template',
            description = paste0('A template describing how the URL should be ',
                                 'constructed from the base URL and input parameters. e.g. ',
                                 '<base_url>/<context>/<input_item>/<search_term>/json.', 
                                 'The url will be constructed by replacing the values ',
                                 'enclosed in <> with the value from corresponding input ',
                                 'parameter of the rest_api object. A term preceeded by . will ',
                                 'assume substitute values from a column name from the  ',
                                 'annotation table.'
            ),
            value = c(
                '<base_url>/<search_by>/<query_column>/property/<property>/JSON'
            ),
            max_length = 1
        ),
        

        property = entity(
            name = 'Compound property',
            description = paste0(
                'A comma separated list of properties to return from the ',
                'pubchem database. ',
                '(see https://pubchem.ncbi.nlm.nih.gov/docs/pug-rest#section=',
                'Compound-Property-Tables for details). Keyword ".all" will ',
                'return all properties.'),
            value = 'InChIKey',
            max_length = 1
        ),
        
        status_codes = entity(
            name = 'Status codes',
            description = paste0(
                'Named list of status codes and function indicating how to ',
                'respond. Should minimally contain a function to parse a ',
                'response for status code 200.'),
            type='list',
            value = list(
                '200' = .parse_json_property,
                '404' = function(...){return(NULL)}
            ),
            max_length = Inf
        )
        
    )
)
    