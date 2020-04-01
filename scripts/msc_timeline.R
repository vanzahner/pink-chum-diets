#devtools::install_github('Kitware/candela', subdir='R/candela')

library(candela)

install.packages("timevis")

library(timevis)

data <- list(
  list(name="Spatial coding", level=1, start=1, end=4),
  list(name="Spatial writing", level=1, start=3, end=4),
  list(name="Spatial edits", level=2, start=4, end=5),
  list(name="Temporal coding", level=1, start=1, end=5),
  list(name="Temporal writing", level=1, start=4, end=6),
  list(name="Temporal editing", level=2, start=6, end=7),
  list(name="Introduction writing", level=1, start=6, end=7),
  list(name="Conclusion writing", level=1, start=7, end=8),
  list(name="Thesis edits", level=2, start=8, end=9),
  list(name="Committee meeting", level=2, start=4, end=5),
  list(name="Defence", level=2, start=9, end=10)
)

candela("GanttChart", data=data, label="name", start="start", end="end",
        level="level", width = 1000, height = 900)

timevis(data=data.frame(
  start = c("2020-01-01", "2020-03-01", "2020-04-01", "2020-01-01", "2020-04-01", "2020-06-01", "2020-06-01", "2020-07-01", "2020-09-01", "2020-04-01", "2020-10-01"),
  end = c("2020-04-01", "2020-04-01", "2020-05-01", "2020-05-01", "2020-06-01", "2020-07-01", "2020-07-01", "2020-08-01", NA, "2020-05-01", NA),
  content=c("Spatial coding", "Spatial writing", "Spatial edits", "Temporal coding", 
            "Temporal writing", "Temporal editing", "Introduction writing", "Conclusion writing", 
            "Thesis edits", "Committee meeting", "Defence"),
  style=c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
  group=c(1, 2, 3, 1, 2, 3, 2, 2, 3, 3, 3)),
  groups=data.frame(id=1:3, content=c("Coding", "Writing", "Other"))
  )

timevis(data=data.frame(
  content=c("Temporal code", "Spatial code",
            "Temporal writing", "Spatial writing", "Conclusion", "Introduction", 
            "Temp. edits", "Spat. edits", "Thesis edits", 
            "Committee meeting", "Defence"),
  start=c("2020-01-01", "2020-01-01",
          "2020-05-01", "2020-03-01", "2020-08-01", "2020-07-01",
          "2020-07-01", "2020-05-01", "2020-09-01",
          "2020-05-01", "2020-10-01"),
  end=c("2020-06-01", "2020-05-01",
        "2020-07-01", "2020-05-01", "2020-09-01", "2020-08-01",
        "2020-08-01", "2020-06-01", "2020-10-01", NA, NA),
  style=c("color: blue;", "color: purple;", "color: blue;", "color: purple;", "color: red;", "color: red;", 
          "color: blue;", "color: purple;", "color: red;", NA, NA),
  group=c(1, 1, 2, 2, 2, 2, 3, 3, 3, 4, 4)),
  groups=data.frame(id=1:4, content=c("Coding", "Writing", "Editing", "Finalizing")),
  showZoom = FALSE)
