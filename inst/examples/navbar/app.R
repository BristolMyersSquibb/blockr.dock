library(blockr.dock)
library(blockr.core)

# Two items that an app appends to the default navbar: a link, which opens in
# a new tab, and a menu that holds a link.
serve(
  new_board(
    blocks = c(a = new_dataset_block("iris")),
    plugins = list(new_dock_plugin(views = list(Page = dock_view("a")))),
    id = "my_board"
  ),
  navbar = custom_navbar(
    c(
      navbar_link(
        "docs",
        "Documentation",
        "https://bristolmyerssquibb.github.io/blockr.dock/"
      ),
      navbar_menu(
        "help",
        "Help",
        navbar_link(
          "issues",
          "Report an issue",
          "https://github.com/BristolMyersSquibb/blockr.dock/issues/new"
        )
      )
    )
  )
)
