install.packages("rjson")
library("rjson")


download_even_posts = function(link){
  even_posts = list()
  id = 1
  while(TRUE){
    next_posts = downloading(link, id)
    if (length(next_posts)==0)
      {break}
    if(id %% 2 == 0)
    {even_posts = append(even_posts,next_posts)
      id=id+1}
    else{id=id+1}
  }
  return(even_posts)
}


downloading = function(link, id){
  full_link = paste(link, id, sep = "")
  j_data = fromJSON(file = full_link)
  return(j_data)
}


download_comments = function(id_list){
  comments = list()
  for (id in id_list){
    comments = append(comments,list(downloading(comments_link,id)))
  }
  return(comments)
}


merge = function(posts,comments)
  {
  merged_list = list()
    for (i in 1:length(posts))
      {
      merged_list = append(merged_list, posts[i])
      for (j in 1:length(comments))
        {
        if (posts[[i]][[2]] == comments[[j]][[1]])
          {
          merged_list[[i]][["comments"]] =  comments[[j]]
          }
        }
       }
  return(merged_list)
}

posts_link = "https://jsonplaceholder.typicode.com/posts?id="
comments_link = "https://jsonplaceholder.typicode.com/comments?postId="
even_posts = download_even_posts(posts_link)
posts_table = data.frame(matrix(unlist(even_posts), nrow=length(even_posts), byrow=T))
id_list = posts_table$X2
comments = download_comments(id_list)
comments_number_vector = sapply(comments, length)
merged_list = merge(even_posts,comments) #z³¹czona lista postów i komentarzy
posts_table$comments = comments_number_vector 
print(posts_table) #tabela postów z liczb¹ komentarzy



