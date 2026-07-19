// More info about Fulma at https://mangelmaxime.github.io/Fulma/
модуль Fulma.Menu

відкрити Fable.React
відкрити Fulma

нехай basic () =
    // Helper to generate a menu item
    нехай menuItem label isActive =
        Menu.Item.li [ Menu.Item.IsActive isActive ]
           [ str label ]
    // Helper to generate a sub menu
    нехай subMenu label isActive children =
        li [ ]
           [ Menu.Item.a [ Menu.Item.IsActive isActive ]
                [ str label ]
             ul [ ] children ]
    // Menu rendering
    Menu.menu [ ]
        [ Menu.label [ ] [ str "General" ]
          Menu.list [ ]
            [ menuItem "Dashboard" false
              menuItem "Customers" false ]
          Menu.label [ ] [ str "Administration" ]
          Menu.list [ ]
            [ menuItem "Team Settings" false
              subMenu "Manage your Team" true
                    [ menuItem "Members" false
                      menuItem "Plugins" false
                      menuItem "Add a член" false ] ]
          Menu.label [ ] [ str "Transactions" ]
          Menu.list [ ]
            [ menuItem "Payments" false
              menuItem "Transfers" false
              menuItem "Balance" false ] ]

div [] [
    Card.card [] [Card.content [] [basic()] ]
] |> mountById "elmish-app"
