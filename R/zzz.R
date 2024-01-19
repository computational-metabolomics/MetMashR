#' @importFrom utils capture.output
#' @importFrom scales manual_pal
get_description=function(id) {
    
    # object template
    M=new_struct(id)
    
    # title
    str=paste0('@title ', M$name)
    
    # get description
    str=c(str,paste0('@description ',M$description))
    
    # add inheritance
    i=.class2(M)
    w=which(i=='struct_class')
    str = c(str,
                '@section ','Inheritance: ', 
                paste0('A `', class(M), 
                '` object inherits the following `struct` classes: \\cr\\cr'),
                paste0('`',i[1:w],'()`',collapse = ' \U2B62 '))
    
    
    # citations
    cits=citations(M)
    cits[length(cits)]=NULL
    for (k in seq_along(cits)) {
        cit=format(cits[[k]],style='text')
        str=c(str,paste0('@references ',cit))
    }
    
    # get libraries
    if (length(M$libraries)>0) {
        str2=paste0('@details ',
                    'This object makes use of functionality from the following packages:',
                    '\\itemize{ '
        )
        for (k in seq_along(M$libraries)) {
            str2=paste0(str2,' \\item{\\code{',M$libraries[k],'}}')
        }
        str2=paste0(str2,'}')
        str=c(str,str2)
    }
    
    # parameters
    P=formals(id)
    
    # for each parameter generate some text
    D=list()
    for (k in seq_along(P)) {
        
        # skip if ellipsis
        if (names(P)[k]=='...') {
            D[[k]]= paste0('@param ',names(P)[k], 
                           ' Additional slots and values passed to \\code{struct_class}.')
            next
        }
        #g=param_obj(M,names(P)[k])
        
        D[[k]]=stringify_params(M,names(P)[k],type='param',val=P[[k]])
    }
    str=c(str,D)
    
    # add outputs and descriptions to value
    O = output_list(M)
    
    D=list()
    for (k in seq_along(O)) {
        D[[k]]=stringify_params(M,names(O)[k],type='output',val=O[[k]])
    }
    
    if (length(D)>0){
        str=c(str,
              paste0('@return ','A  \\code{', class(M)[1],'} object with the following \\code{output} slots:' ),
              '\\tabular{ll}{',
              D,
              '}')
    } else{
        str=c(str,'@return ','A  \\code{', class(M)[1],'} object. This object has no \\code{output} slots.')
        if (is(M,'chart')) {
            str=c(str, 'See \\code{\\link[struct]{chart_plot}} in the \\code{struct} package to plot this chart object.')
        }
        
    }
    
    # basic example
    eg = '@examples'
    cd = as.code(M,mode='neat',quiet=TRUE)
    cd = gsub('[a list]','list()',cd,fixed = TRUE)
    cd = gsub('[a function]','function(){}',cd,fixed = TRUE)
    cd = gsub('[a annotation_database]','annotation_database()',cd,fixed = TRUE)
    cd = gsub('[a logical]','FALSE',cd,fixed = TRUE)
    cd = gsub('[a data.frame]','data.frame(id=NA)',cd,fixed = TRUE)
    str=c(str,eg,cd,'')
    
    
    return(unlist(str))
}




stringify_params = function(M,P,type='param',val=NULL) {
    
    # get parameter as an object
    if (type=='param') {
        p = param_obj(M,P)
    } else {
        p=output_obj(M,P)
    }
    
    # if its an entity object then get its description
    if (is(p,'entity')) {
        d = p$description
        # ensure first character is upper case and last character is a fullstop.
        d=unlist(lapply(d,function(x){
            # first is upper
            substr(x,1,1) = toupper(substr(x,1,1))
            # last is .
            if (substr(x,nchar(x),nchar(x)) != '.') {
                x=paste0(x,'.')
            }
            return(x)
        }))
        
        # if d has more than one entry and is a named vector then...
        if (length(d)>1) {
            # if it has names then
            if (!is.null(names(d))) {
                # create a named list
                it_list='\\itemize{ '
                for (j in seq_along(d)) {
                    it_list=paste0(it_list,'\\item{\\code{"',names(d)[j],'"}: ',d[j],'}')
                }
            } else {
                # no names so use a bulleted list
                it_list='\\itemize{'
                for (j in seq_along(d)) {
                    it_list=paste0(it_list,'\\item{',d[j],'}')
                }
            }
            
            # add list
            it_list=paste0(it_list,'}')
            d=paste0(p$name,'. Allowed values are limited to the following: ',it_list)
        }
        
        # add the allowed types
        t = p$type
        
    } else {
        # if not an entity then there is no description
        d = ''
        t=class(val)[[1]]
    }
    # collapse if more than 1
    t=paste0(t,collapse=', ')
    # enclose in brackets
    t=paste0('(',t,') ')
    # add to description
    d=paste0(t,d)
    
    # if the parameter has a default, then add on the text.
    if ( (!is(val,'name')) & type=='param'){
        d=paste0(d, ' The default is ')
        if (length(val)>1 & !is.call(val)) {
            d=paste0(d,'\\code{',capture.output(val)[1],'}.')
        } else {
            if (is.null(val)) {
                d=paste0(d,'\\code{NULL}.')
            } else if (is(val,'character')) {
                d=paste0(d,'\\code{"',val,'"}.')
            } else if (is.function(val) | is.call(val)) {
                d=paste0(d,'\\code{',gsub('}','\\}',
                                          paste0(trimws(deparse(val)),collapse='')),'}.')
                #d=paste0(d,'\\code{some_function()}.')
            } else {
                
                d=paste0(d,'\\code{',val,'}.\\cr')
            }
        }
    } else {
        # no default is provided
    }
    if (type=='param') {
        OUT=paste0('@', type, ' ', P, ' ', d)
    } else {
        OUT=paste0('\\code{',P,'} \\tab          ',d,' \\cr')
    }
}


scale_fill_Publication <- function(...){
    #library(scales)
    discrete_scale("fill","Publication",scales::manual_pal(
        values = c(
            "#1f78b4","#e31a1c","#33a02c","#ff7f00","#6a3d9a","#b15928",
            "#a6cee3","#fb9a99","#b2df8a","#fdbf6f","#cab2d6","#ffff99")), ...)
}

scale_color_Publication <- function(...){
    #library(scales)
    discrete_scale("color","Publication",scales::manual_pal(
        values = c(
            "#1f78b4","#e31a1c","#33a02c","#ff7f00","#6a3d9a","#b15928",
            "#a6cee3","#fb9a99","#b2df8a","#fdbf6f","#cab2d6","#ffff99")), ...)
}

#' @import ggthemes
theme_Publication <- function(base_size=14){ #, base_family="helvetica") {
    (theme_foundation(base_size=base_size) #, base_family=base_family)
     + theme(plot.title = element_text(face = "bold",
                                       size = rel(1.2), hjust = 0.5),
             text = element_text(),
             panel.background = element_rect(colour = '#ffffff'),
             plot.background = element_rect(colour = '#ffffff'),
             panel.border = element_rect(colour = NA),
             axis.title = element_text(face = "bold",size = rel(1)),
             axis.title.y = element_text(angle=90,vjust =2),
             axis.title.x = element_text(vjust = -0.2),
             axis.text = element_text(),
             axis.line = element_line(colour="black"),
             axis.ticks = element_line(),
             panel.grid.major = element_line(colour="#f0f0f0"),
             panel.grid.minor = element_blank(),
             legend.key = element_rect(colour = '#ffffff'),
             legend.key.size= unit(0.75, "cm"),
             legend.spacing = unit(0.2, "cm"),
             legend.title = element_text(face="italic"),
             plot.margin=unit(c(10,5,5,5),"mm"),
             strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
             strip.text = element_text(face="bold")
     ))
    
}


# override default entity value without need to provide name, description etc 
# again
.set_entity_value = function(
        obj, 
        param_id,
        ...){
    # create obj
    obj = new_struct(obj)
    # get entity
    E = param_obj(obj,param_id)
    # set values
    L = list(...)
    for (k in names(L)) {
        slot(E,k) = L[[k]]
    }
    # return entity
    return(E)
}


