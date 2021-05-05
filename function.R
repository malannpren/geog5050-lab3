ex03_func <- function(x1, y1, x2, y2) {
    manhattan <- abs(x1 - x2) + abs(y1 - y2)
    euclidean <- sqrt((x2 - x1)**2 + (y2 - y1)**2)
    cat("Point 1: (", x1, ", ", y1, ")",
        "\nPoint 2: (", x2, ", ", y2, ")",
        "\nManhattan Distance: ", manhattan,
        "\nEuclidean Distance: ", euclidean)
}