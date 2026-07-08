# str_value.dock_board renders every section

    Code
      cat(str_value(board))
    Output
      <dock_board>
      <blocks[2]>
        a: <dataset_block> dataset*, package
        b: <head_block> n, direction
      <links[1]>
        ab: <link> a -> b (data)
      <stacks[1]>
        grp: <dock_stack> "Group A": a, b Color: "#ff0000"
      <board_options[1]>
        board_name: NULL
      <dock_views[2]>
        one: <dock_view> a
        two: <dock_view> b (active)
      <dock_extensions[1]>
        edit_board_extension: <edit_board_extension>

# str_value.dock_board renders empty sections

    Code
      cat(str_value(board))
    Output
      <dock_board>
      <blocks[1]>
        a: <dataset_block> dataset*, package
      <links[0]>
      <stacks[0]>
      <board_options[1]>
        board_name: NULL
      <dock_views[1]>
        main: <dock_view> a (active)
      <dock_extensions[0]>

# str() on a dock_board displays via the inherited str.board

    Code
      str(board)
    Output
       <dock_board>
      <blocks[1]>
        a: <dataset_block> dataset*, package
      <links[0]>
      <stacks[0]>
      <board_options[1]>
        board_name: NULL
      <dock_views[1]>
        main: <dock_view> a (active)
      <dock_extensions[1]>
        edit_board_extension: <edit_board_extension>

