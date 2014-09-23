# text for Acknowledgment tab in 'names' shiny app

    str1 <- "Data source, U.S. Social Security Administration"
    str2 <- "http://www.ssa.gov/oact/babynames/limits.html"
    str3 <- "The 'shiny' package was authored by the RStudio Team."
    str4 <- "The 'babynames' R package was authored by Hadley Wickham."
    str5 <- "The babynames shiny scripts from Garrett Grolemund
were used as baseline for this shiny app."
    str6 <- "Acknowledgements from the Social Security Administration:"
    str7 <- "1. Names are restricted to cases where the year of birth, sex, 
    State of birth (50 States and District of Columbia) are on record, and
    where the given name is at least 2 characters long."
    str8 <- "2. Name data are not edited. For example, the sex associated with 
    a name may be incorrect. Entries such as 'Unknown' and 'Baby' are not 
    removed from the lists."
    str9 <- "3. Different spellings of similar names are not combined. For 
    example, the names Caitlin, Caitlyn, Kaitlin, Kaitlyn, Kaitlynn, Katelyn, 
    and Katelynn are considered separate names and each has its own rank."
    str10 <- "4. When two different names are tied with the same frequency for 
    a gven year of birth, we break the tie by assigning rank in alphabetical 
    order."
    str11 <- "5. Some names are applied to both males and females (for example, 
    Micah). Our rankings are done by sex, so that a name such as Micah will
    have a different rank for males as compared to females. When you seek 
    the popularity of a specific name (see 'Popularity of a Name'), you can 
    specify the sex. If you do not specify the sex, we provide rankings for 
    the more popular name-sex combination."

    blk1 <- HTML(paste(str1, str2, sep = '<br/>'))
    blk2 <- HTML(paste(str3, str4, str5, sep = '<br/>'))
    blk3 <- HTML(paste(str6,str7,str8,str9,str10, str11, sep = '<br/>'))
