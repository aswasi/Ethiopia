pullAttributes <- function (data) {
  label = lapply(data, function(x) attr(x, 'label'))
  
  label = data.frame(label)
  # labels = lapply(data, function(x) attr(x, 'labels'))
  
  # attrs = data.frame(label = label, labels = labels)
}