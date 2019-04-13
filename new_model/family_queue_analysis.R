# Analyze the queue for inprocessing by family type. #


arrivals <- get_mon_arrivals(env,per_resource=TRUE)
resources <- get_mon_resources(env)

r <- resources[
  which(resources$resource %in% paste("Inprocessing",family.status.probs[,1],sep="_")),
]

r$resource <- factor(
  r$resource,
  levels = rev(
    paste(
      "Inprocessing",
      c(
        "Ch/Accompanied",
        "F/Accompanied",
        "M/Accompanied",
        "Ch/Unaccompanied",
        "F/Unaccompanied",
        "M/Unaccompanied"
      ),
    sep = "_"
    )
  )
)

g <- ggplot(
  data = r,
  mapping = aes(
    x = time/24,
    y = server+1,
    color = resource,
    group = resource
  )
) +
  geom_line(size=1) +
  labs(
    x = "Time (days)",
    y = "Quantity",
    title = "Inprocessing Queue Length"
  ) +
  scale_y_continuous(
    trans = "log10"
  ) + 
  scale_color_manual(
    values = c(
      "Inprocessing_Ch/Accompanied" = "black",
      "Inprocessing_F/Accompanied" = "green",
      "Inprocessing_M/Accompanied" = "brown",
      "Inprocessing_Ch/Unaccompanied" = "blue",
      "Inprocessing_F/Unaccompanied" = "orange",
      "Inprocessing_M/Unaccompanied" = "red"
    ),
    breaks = rev(
      c(
        "Inprocessing_Ch/Accompanied",
        "Inprocessing_F/Accompanied",
        "Inprocessing_M/Accompanied",
        "Inprocessing_Ch/Unaccompanied",
        "Inprocessing_F/Unaccompanied",
        "Inprocessing_M/Unaccompanied"
      )
    ),
    labels = rev(
      c(
        "Ch/Accompanied",
        "F/Accompanied",
        "M/Accompanied",
        "Ch/Unaccompanied",
        "F/Unaccompanied",
        "M/Unaccompanied"
      )
    ),
    name = "Family Status"
  )

print(g)

