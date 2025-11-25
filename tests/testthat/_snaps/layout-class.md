# panel layout

    Code
      draw_panel_tree(NULL)
    Output
      $root
      $root$type
      [1] "branch"
      
      $root$data
      list()
      
      $root$size
      [1] 0
      
      
      $orientation
      [1] "HORIZONTAL"
      

---

    Code
      draw_panel_tree(c("a", "b", "c"))
    Output
      $root
      $root$type
      [1] "branch"
      
      $root$data
      $root$data[[1]]
      $root$data[[1]]$type
      [1] "leaf"
      
      $root$data[[1]]$data
      $root$data[[1]]$data$views
      $root$data[[1]]$data$views[[1]]
      [1] "a"
      
      
      $root$data[[1]]$data$activeView
      [1] "a"
      
      $root$data[[1]]$data$id
      [1] "1"
      
      
      $root$data[[1]]$size
      [1] 0.3333333
      
      
      $root$data[[2]]
      $root$data[[2]]$type
      [1] "leaf"
      
      $root$data[[2]]$data
      $root$data[[2]]$data$views
      $root$data[[2]]$data$views[[1]]
      [1] "b"
      
      
      $root$data[[2]]$data$activeView
      [1] "b"
      
      $root$data[[2]]$data$id
      [1] "2"
      
      
      $root$data[[2]]$size
      [1] 0.3333333
      
      
      $root$data[[3]]
      $root$data[[3]]$type
      [1] "leaf"
      
      $root$data[[3]]$data
      $root$data[[3]]$data$views
      $root$data[[3]]$data$views[[1]]
      [1] "c"
      
      
      $root$data[[3]]$data$activeView
      [1] "c"
      
      $root$data[[3]]$data$id
      [1] "3"
      
      
      $root$data[[3]]$size
      [1] 0.3333333
      
      
      
      $root$size
      [1] 0.3333333
      
      
      $orientation
      [1] "HORIZONTAL"
      

---

    Code
      draw_panel_tree(list("a", list("b", "c")))
    Output
      $root
      $root$type
      [1] "branch"
      
      $root$data
      $root$data[[1]]
      $root$data[[1]]$type
      [1] "leaf"
      
      $root$data[[1]]$data
      $root$data[[1]]$data$views
      $root$data[[1]]$data$views[[1]]
      [1] "a"
      
      
      $root$data[[1]]$data$activeView
      [1] "a"
      
      $root$data[[1]]$data$id
      [1] "1"
      
      
      $root$data[[1]]$size
      [1] 0.5
      
      
      $root$data[[2]]
      $root$data[[2]]$type
      [1] "branch"
      
      $root$data[[2]]$data
      $root$data[[2]]$data[[1]]
      $root$data[[2]]$data[[1]]$type
      [1] "leaf"
      
      $root$data[[2]]$data[[1]]$data
      $root$data[[2]]$data[[1]]$data$views
      $root$data[[2]]$data[[1]]$data$views[[1]]
      [1] "b"
      
      
      $root$data[[2]]$data[[1]]$data$activeView
      [1] "b"
      
      $root$data[[2]]$data[[1]]$data$id
      [1] "2"
      
      
      $root$data[[2]]$data[[1]]$size
      [1] 0.5
      
      
      $root$data[[2]]$data[[2]]
      $root$data[[2]]$data[[2]]$type
      [1] "leaf"
      
      $root$data[[2]]$data[[2]]$data
      $root$data[[2]]$data[[2]]$data$views
      $root$data[[2]]$data[[2]]$data$views[[1]]
      [1] "c"
      
      
      $root$data[[2]]$data[[2]]$data$activeView
      [1] "c"
      
      $root$data[[2]]$data[[2]]$data$id
      [1] "3"
      
      
      $root$data[[2]]$data[[2]]$size
      [1] 0.5
      
      
      
      $root$data[[2]]$size
      [1] 0.5
      
      
      
      $root$size
      [1] 0.5
      
      
      $orientation
      [1] "HORIZONTAL"
      

