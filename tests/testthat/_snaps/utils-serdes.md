# serialized dock_layouts records view id, name and active

    Code
      cat(jsonlite::toJSON(layouts, pretty = TRUE, auto_unbox = TRUE, null = "null"))
    Output
      {
        "object": "dock_layouts",
        "payload": {
          "active": "view_two",
          "views": {
            "view_one": {
              "object": "dock_layout",
              "payload": {
                "orientation": "horizontal",
                "children": [
                  "block_panel-a"
                ]
              },
              "name": "Analysis"
            },
            "view_two": {
              "object": "dock_layout",
              "payload": {
                "orientation": "horizontal",
                "children": [
                  "block_panel-b"
                ]
              },
              "name": "Overview"
            }
          }
        }
      }

