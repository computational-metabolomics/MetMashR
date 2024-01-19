#' @eval get_description('annotation_pie_chart')
#' @export 
annotation_pie_chart = function(
        factor_name,
        label_rotation = FALSE,
        label_location = 'inside',
        label_type = 'percent',
        legend = FALSE,
        pie_rotation = 0,
        centre_radius = 0,
        centre_label = NULL,
        count_na = FALSE,
        ...) {
    
    check = centre_radius>=0 & centre_radius <= 1
    if (!check){
        stop('Pie chart centre_radius must be between 0 and 1.')
    }
    
    out=struct::new_struct(
        'annotation_pie_chart',
        factor_name = factor_name,
        label_rotation = label_rotation,
        label_location = label_location,
        legend = legend,
        pie_rotation = pie_rotation,
        label_type = label_type,
        centre_radius = centre_radius,
        centre_label = centre_label,
        count_na = count_na,
        ...)
    
    return(out)
}


.annotation_pie_chart<-setClass(
    "annotation_pie_chart",
    
    contains='chart',
    
    slots=c(
        factor_name = 'entity',
        label_rotation = 'entity',
        label_location = 'enum',
        label_type = 'enum',
        legend = 'entity',
        pie_rotation = 'entity',
        centre_radius = 'entity',
        centre_label = 'entity',
        count_na = 'entity'
    ),
    
    prototype = list(
        name='Annotation pie chart',
        description=paste0(
            'Display a pie chart of labels in the specified column of an ',
            'annotation_source.'),
        type="image",
        .params=c('factor_name','label_location','label_rotation','legend',
                  'pie_rotation','label_type','centre_radius','centre_label',
                  'count_na'),
        
        libraries = 'ggplot2',
        
        factor_name = entity(
            name = 'Factor name',
            description = paste0(
                'The name of the column in the `annotation_source` to ',
                'generate a pie chart from.'),
            type = 'character',
            value = 'V1',
            max_length = 1
        ),
        
        label_type = enum(
            name = 'Label type',
            description = c(
                'percent' = 'Labels will include the percentage for the segment.',
                'count' = 'Labels will include the count for the segment.',
                'none' = 'Labels will not include extra information.'
            ),
            value = 'percent',
            max_length = 1,
            type = 'character',
            allowed = c('percent','count','none')
        ),
        
        label_rotation = entity(
            name = 'Rotate labels',
            description = c(
                `TRUE` = 'Rotate labels to match segments.',
                `FALSE` = 'Do not rotate labels.'),
            type = 'logical',
            value = TRUE,
            max_length = 1
        ),
        
        label_location = enum(
            name = 'Label location',
            description = c(
                inside = 'Labels are displayed inside the segments.',
                outside = 'Labels are displayed outside the segments.'),
            type = 'character',
            value = 'inside',
            max_length = 1,
            allowed = c('inside','outside')
        ),
        
        legend = entity(
            name = 'Display legend',
            description = c(
                `TRUE` = 'Groups are indicated using a legend.',
                `FALSE` = 'Groups are indicated in the labels.'),
            type = 'logical',
            value = TRUE,
            max_length = 1
        ),
        
        pie_rotation = entity(
            name = 'Pie rotation',
            description = paste0(
                'The number of degrees to rotate the pie chart by, clockwise'),
            type = 'numeric',
            value = 0,
            max_length = 1
        ),
        
        centre_radius = entity(
            name = 'Centre radius',
            description = paste0(
                'The radius of the centre circle. Used to make a "donut" ',
                'plot. Should be a value between 0 and 1.'
            ),
            value = 0,
            type = c('numeric','integer'),
            max_length = 1
        ),
        
        centre_label = entity(
            name = 'Centre text',
            description = paste0(
                'The text to display in the centre of the pie chart. Mostly ',
                'used with donut plots where `centre_radius` is greater than 0.'
            ),
            value = NULL,
            type = c('NULL','character'),
            max_length = 1
        ),
        
        count_na = entity(
            name = 'Count missing values',
            description = paste0(
                'Include the number of missing values in the pie chart.'
            ),
            value = FALSE,
            type = 'logical',
            max_length = 1
        )
        
    )
)

#' @export
setMethod(f="chart_plot",
          signature=c("annotation_pie_chart",'annotation_source'),
          definition=function(obj,dobj) {
              
              r1 = obj$centre_radius
              radians = obj$pie_rotation * pi/180
              
              if (obj$label_location == 'inside') {
                  offset = 1
              } else {
                  offset = 1.6
              }
              
              if (obj$count_na) {
                  dobj$data[[obj$factor_name]] = 
                      factor(dobj$data[[obj$factor_name]])
              }
              # counts from column of labels
              df = dobj$data %>% 
                  group_by(.data[[obj$factor_name]]) %>%
                  summarise(count = n())
              
              # labels
              df$label=''
              if (obj$label_type == 'percent') {
                  df$label = paste0(
                      round(df$count/sum(df$count)*100,digits=1),'%')
              } else if (obj$label_type == 'count') {
                  df$label = as.character(df$count)
              }
              
              if (!obj$legend) {
                  if (obj$label_type != 'none') {
                      df$label = paste0(df[[obj$factor_name]], 
                                        ' (', df$label, ')')
                  } else {
                      df$label = df[[obj$factor_name]]
                  }
              }
              
              # position of label in degrees
              df$angle = ((cumsum(df$count)-(df$count/2))/sum(df$count))*360 -
                  obj$pie_rotation
              # right align labels if left of circle
              df$hjust = as.numeric(df$angle<180) 
              # centre if at top or bottom
              df$hjust[df$angle >175 & df$angle<185] = 0.5
              df$hjust[df$angle >355 | df$angle<5] = 0.5
              
              if (!obj$label_rotation) {
                  df$rotate = 0
              } else {
                  df$rotate = df$angle-270-(180*df$hjust)
              }
              
              # plot
              g = ggplot(
                  data = df, 
                  aes(x=1+(r1/2),y=.data[['count']],fill=.data[[obj$factor_name]]))+
                  geom_bar(width = 1-r1,colour = 'white',stat='identity',linewidth = 0.5)+
                  coord_polar("y",start = radians, clip = 'off') + 
                  theme_void() +
                  geom_text(aes(x=offset,label = .data[['label']],angle=rotate), 
                            position = position_stack(vjust=0.5),
                            hjust=df$hjust,) +
                  theme(plot.margin = unit(c(1, 1, 1, 1), "lines")) +
                  xlim(c(0.5,1.6)) +
                  scale_fill_Publication()
              
              # legend
              if (!obj$legend) {
                  g = g + theme(legend.position = 'none')
              }
              
              # add centre label
              if (!is.null(obj$centre_label)) {
                  if (obj$centre_label == '.total') {
                      obj$centre_label = paste0(
                          'Total\n',sum(df$count,na.rm = TRUE))
                  }
                  g = g +
                      geom_text(aes(x = 0.5, y=0),label=obj$centre_label)
              }
              
              
              return(g)
          }
)
