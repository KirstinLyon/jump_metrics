

# Check for DK clubs - look at UNKNOWN
club <- events |> 
    select(club, country) |> 
    distinct()

# Check for DK clubs - look for unknown countries and names
DK_name_club <- events |> 
    select(name, club, country) |> 
    distinct() |> 
    filter(country == "DEN", club != "unknown")

#club <- kickout::data_representing_map