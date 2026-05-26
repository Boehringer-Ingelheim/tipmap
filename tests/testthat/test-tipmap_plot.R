test_that("tipmap_plot returns ggplot without meta-analysis", {
  tipmap_data <- load_tipmap_data("tipdat.rds")
  p <- tipmap_plot(tipmap_data)
  
  expect_s3_class(p, "ggplot")
})

test_that("tipmap_plot returns ggplot with custom axis settings", {
  tipmap_data <- load_tipmap_data("tipdat.rds")
  p <- tipmap_plot(
    tipmap_data,
    y_range = c(-5, 5),
    y_breaks = seq(-5, 5, by = 1)
  )
  
  expect_s3_class(p, "ggplot")
})

test_that("tipmap_plot rejects invalid inputs", {
  tipmap_data <- load_tipmap_data("tipdat.rds")
  
  expect_error(tipmap_plot(1), "tipmap_data")
  expect_error(tipmap_plot(tipmap_data, null_effect = c(0, 1)), "length 1")
  expect_error(tipmap_plot(tipmap_data, y_range = 1), "length 2")
})

test_that("mapping correct", {
  plot <- tipmap_plot(load_tipmap_data("tipdat.rds"))
  layer_mappings <- lapply(plot$layers, function(x) as.character(x$mapping))
  
  expect_equal(layer_mappings[[1]], c("~yintercept"))
  expect_equal(layer_mappings[[3]], c("~t.0.025", "~t.0.975"))
  expect_equal(layer_mappings[[4]], c("~t.est"))
  expect_equal(layer_mappings[[5]], c("~t.0.025", "2.5%/97.5%", "2.5%/97.5%"))
  expect_equal(layer_mappings[[6]], c("~t.0.05", "5%/95%", "5%/95%"))
  expect_equal(layer_mappings[[7]], c("~t.0.1", "10%/90%", "10%/90%"))
  expect_equal(layer_mappings[[8]], c("~t.0.2", "20%/80%", "20%/80%"))
  expect_equal(layer_mappings[[9]], c("~t.est", "50%", "50%"))
  expect_equal(layer_mappings[[10]], c("~t.0.8", "20%/80%", "20%/80%"))
  expect_equal(layer_mappings[[11]], c("~t.0.9", "10%/90%", "10%/90%"))
  expect_equal(layer_mappings[[12]], c("~t.0.95", "5%/95%", "5%/95%"))
  expect_equal(layer_mappings[[13]], c("~t.0.975", "2.5%/97.5%", "2.5%/97.5%"))
  
  expect_equal(layer_mappings[[14]], c("~tippingPoint.025", "~t.0.025[x.at == tippingPoint.025]"))
  expect_equal(layer_mappings[[15]], c("~tippingPoint.025"))
  expect_equal(layer_mappings[[16]], c("~tippingPoint.05", "~t.0.05[x.at == tippingPoint.05]"))
  expect_equal(layer_mappings[[17]], c("~tippingPoint.05"))
  expect_equal(layer_mappings[[18]], c("~tippingPoint.1", "~t.0.1[x.at == tippingPoint.1]"))
  expect_equal(layer_mappings[[19]], c("~tippingPoint.1"))
  expect_equal(layer_mappings[[20]], c("~tippingPoint.2", "~t.0.2[x.at == tippingPoint.2]"))
  expect_equal(layer_mappings[[21]], c("~tippingPoint.2"))
  
  expect_length(layer_mappings, 21)
})

test_that("annotation layer mappings are present when tipping points are added", {
  tipmap_data_with_annotations <- data.frame(
    x.at = c(-0.15, 0.2, 0.4, 0.6, 0.8, 1.15),
    x.col = factor(c("new.obs", "post", "post", "post", "post", "prior")),
    t.est = c(1.2, 0.9, 0.7, 0.5, 0.3, 1.1),
    t.0.025 = c(0.8, 0.30, 0.10, -0.20, -0.40, 0.9),
    t.0.05  = c(0.9, 0.35, 0.12, -0.15, -0.35, 1.0),
    t.0.1   = c(1.0, 0.40, 0.15, -0.10, -0.30, 1.1),
    t.0.2   = c(1.1, 0.45, 0.20, -0.05, -0.25, 1.2),
    t.0.8   = c(1.3, 1.10, 0.95, 0.80, 0.65, 1.4),
    t.0.9   = c(1.4, 1.20, 1.05, 0.90, 0.75, 1.5),
    t.0.95  = c(1.5, 1.30, 1.15, 1.00, 0.85, 1.6),
    t.0.975 = c(1.6, 1.40, 1.25, 1.10, 0.95, 1.7)
  )
  
  plot <- tipmap_plot(tipmap_data_with_annotations)
  layer_mappings <- lapply(plot$layers, function(x) as.character(x$mapping))
  
  # base layers still present
  expect_equal(layer_mappings[[1]], c("~yintercept"))
  expect_equal(layer_mappings[[3]], c("~t.0.025", "~t.0.975"))
  expect_equal(layer_mappings[[4]], c("~t.est"))
  expect_equal(layer_mappings[[5]], c("~t.0.025", "2.5%/97.5%", "2.5%/97.5%"))
  expect_equal(layer_mappings[[6]], c("~t.0.05", "5%/95%", "5%/95%"))
  expect_equal(layer_mappings[[7]], c("~t.0.1", "10%/90%", "10%/90%"))
  expect_equal(layer_mappings[[8]], c("~t.0.2", "20%/80%", "20%/80%"))
  expect_equal(layer_mappings[[9]], c("~t.est", "50%", "50%"))
  expect_equal(layer_mappings[[10]], c("~t.0.8", "20%/80%", "20%/80%"))
  expect_equal(layer_mappings[[11]], c("~t.0.9", "10%/90%", "10%/90%"))
  expect_equal(layer_mappings[[12]], c("~t.0.95", "5%/95%", "5%/95%"))
  expect_equal(layer_mappings[[13]], c("~t.0.975", "2.5%/97.5%", "2.5%/97.5%"))
  
  # annotation layers: point + vline for 0.025, 0.05, 0.1, 0.2
  expect_equal(layer_mappings[[14]], c("~tippingPoint.025", "~t.0.025[x.at == tippingPoint.025]"))
  expect_equal(layer_mappings[[15]], c("~tippingPoint.025"))
  
  expect_equal(layer_mappings[[16]], c("~tippingPoint.05", "~t.0.05[x.at == tippingPoint.05]"))
  expect_equal(layer_mappings[[17]], c("~tippingPoint.05"))
  
  expect_equal(layer_mappings[[18]], c("~tippingPoint.1", "~t.0.1[x.at == tippingPoint.1]"))
  expect_equal(layer_mappings[[19]], c("~tippingPoint.1"))
  
  expect_equal(layer_mappings[[20]], c("~tippingPoint.2", "~t.0.2[x.at == tippingPoint.2]"))
  expect_equal(layer_mappings[[21]], c("~tippingPoint.2"))
  
  expect_length(layer_mappings, 21)
})

test_that("tipmap_plot reports MAP prior interval including null", {
  tipmap_data <- data.frame(
    x.at = c(-0.15, 0.2, 0.4, 1.15),
    x.col = factor(c("new.obs", "post", "post", "prior")),
    t.est = c(1.0, 0.8, 0.6, 0.1),
    t.0.025 = c(0.5, 0.2, 0.1, -0.2),
    t.0.05 = c(0.6, 0.3, 0.2, -0.1),
    t.0.1 = c(0.7, 0.4, 0.3, 0.0),
    t.0.2 = c(0.8, 0.5, 0.4, 0.05),
    t.0.8 = c(1.2, 1.0, 0.9, 0.2),
    t.0.9 = c(1.3, 1.1, 1.0, 0.3),
    t.0.95 = c(1.4, 1.2, 1.1, 0.4),
    t.0.975 = c(1.5, 1.3, 1.2, 0.5)
  )
  
  expect_message(
    tipmap_plot(tipmap_data, null_effect = 0),
    "95% credible interval for MAP prior includes null treatment effect"
  )
})

test_that("tipmap_plot reports target population without borrowing", {
  tipmap_data <- data.frame(
    x.at = c(-0.15, 0.2, 0.4, 1.15),
    x.col = factor(c("new.obs", "post", "post", "prior")),
    t.est = c(1.0, 0.8, 0.6, 0.1),
    t.0.025 = c(0.2, 0.2, 0.1, -0.2),
    t.0.05 = c(0.3, 0.3, 0.2, -0.1),
    t.0.1 = c(0.4, 0.4, 0.3, 0.0),
    t.0.2 = c(0.5, 0.5, 0.4, 0.05),
    t.0.8 = c(1.2, 1.0, 0.9, 0.2),
    t.0.9 = c(1.3, 1.1, 1.0, 0.3),
    t.0.95 = c(1.4, 1.2, 1.1, 0.4),
    t.0.975 = c(1.5, 1.3, 1.2, 0.5)
  )
  
  expect_message(
    tipmap_plot(tipmap_data, null_effect = 0),
    "Treatment effect in target population without borrowing"
  )
})

test_that("tipmap_plot works with meta-analysis row present", {
  tipmap_data_meta <- data.frame(
    x.at = c(-0.15, 0.2, 0.4, 0.6, 1.15, 1.35),
    x.col = factor(c("new.obs", "post", "post", "post", "prior", "prior")),
    t.est = c(1.2, 1.0, 0.8, 0.6, 1.1, 1.05),
    t.0.025 = c(0.8, 0.4, 0.2, 0.1, -0.2, 0.2),
    t.0.05  = c(0.9, 0.5, 0.3, 0.2, -0.1, NA),
    t.0.1   = c(1.0, 0.6, 0.4, 0.3,  0.0, NA),
    t.0.2   = c(1.1, 0.7, 0.5, 0.4,  0.1, NA),
    t.0.8   = c(1.3, 1.2, 1.0, 0.9,  0.8, NA),
    t.0.9   = c(1.4, 1.3, 1.1, 1.0,  0.9, NA),
    t.0.95  = c(1.5, 1.4, 1.2, 1.1,  1.0, NA),
    t.0.975 = c(1.6, 1.5, 1.3, 1.2,  1.1, 1.3)
  )
  
  p <- tipmap_plot(tipmap_data_meta)
  
  expect_s3_class(p, "ggplot")
})

test_that("tipmap_plot adds upper-tail annotation layers", {
  tipmap_data_upper <- data.frame(
    x.at = c(-0.15, 0.2, 0.4, 0.6, 0.8, 1.15),
    x.col = factor(c("new.obs", "post", "post", "post", "post", "prior")),
    t.est = c(-1.2, -0.9, -0.7, -0.5, -0.3, -1.1),
    t.0.025 = c(-1.6, -1.40, -1.25, -1.10, -0.95, -1.7),
    t.0.05  = c(-1.5, -1.30, -1.15, -1.00, -0.85, -1.6),
    t.0.1   = c(-1.4, -1.20, -1.05, -0.90, -0.75, -1.5),
    t.0.2   = c(-1.3, -1.10, -0.95, -0.80, -0.65, -1.4),
    t.0.8   = c(-1.1, -0.45, -0.20,  0.05,  0.25, -1.2),
    t.0.9   = c(-1.0, -0.40, -0.15,  0.10,  0.30, -1.1),
    t.0.95  = c(-0.9, -0.35, -0.12,  0.15,  0.35, -1.0),
    t.0.975 = c(-0.8, -0.30, -0.10,  0.20,  0.40, -0.9)
  )
  
  plot <- tipmap_plot(tipmap_data_upper)
  layer_mappings <- lapply(plot$layers, function(x) as.character(x$mapping))
  
  expect_equal(layer_mappings[[14]], c("~tippingPoint.975", "~t.0.975[x.at == tippingPoint.975]"))
  expect_equal(layer_mappings[[15]], c("~tippingPoint.975"))
  expect_equal(layer_mappings[[16]], c("~tippingPoint.95", "~t.0.95[x.at == tippingPoint.95]"))
  expect_equal(layer_mappings[[17]], c("~tippingPoint.95"))
  expect_equal(layer_mappings[[18]], c("~tippingPoint.9", "~t.0.9[x.at == tippingPoint.9]"))
  expect_equal(layer_mappings[[19]], c("~tippingPoint.9"))
  expect_equal(layer_mappings[[20]], c("~tippingPoint.8", "~t.0.8[x.at == tippingPoint.8]"))
  expect_equal(layer_mappings[[21]], c("~tippingPoint.8"))
  
  expect_length(layer_mappings, 21)
})

test_that("tipmap_plot does not add annotation layers when tipping points are zero", {
  tipmap_data_zero_tp <- data.frame(
    x.at = c(-0.15, 0, 0.4, 0.8, 1.15),
    x.col = factor(c("new.obs", "post", "post", "post", "prior")),
    t.est = c(1.2, 0.8, 0.7, 0.6, 1.1),
    t.0.025 = c(0.9, 0.01, 0.3, 0.5, 0.8),
    t.0.05  = c(1.0, 0.01, 0.4, 0.6, 0.9),
    t.0.1   = c(1.1, 0.01, 0.5, 0.7, 1.0),
    t.0.2   = c(1.2, 0.01, 0.6, 0.8, 1.1),
    t.0.8   = c(1.4, 1.0, 1.1, 1.2, 1.3),
    t.0.9   = c(1.5, 1.1, 1.2, 1.3, 1.4),
    t.0.95  = c(1.6, 1.2, 1.3, 1.4, 1.5),
    t.0.975 = c(1.7, 1.3, 1.4, 1.5, 1.6)
  )
  
  plot <- tipmap_plot(tipmap_data_zero_tp)
  expect_length(plot$layers, 13)
})

test_that("tipmap_plot applies optional y_range and y_breaks", {
  tipmap_data <- load_tipmap_data("tipdat.rds")
  
  p <- tipmap_plot(
    tipmap_data,
    y_range = c(-2, 2),
    y_breaks = seq(-2, 2, by = 0.5)
  )
  
  expect_s3_class(p, "ggplot")
})

test_that("tipmap_plot reports MAP prior interval including null", {
  tipmap_data <- data.frame(
    x.at = c(-0.15, 0.2, 0.4, 1.15),
    x.col = factor(c("new.obs", "post", "post", "prior")),
    t.est = c(1.0, 0.8, 0.6, 0.1),
    t.0.025 = c(0.5, 0.2, 0.1, -0.2),
    t.0.05 = c(0.6, 0.3, 0.2, -0.1),
    t.0.1 = c(0.7, 0.4, 0.3, 0.0),
    t.0.2 = c(0.8, 0.5, 0.4, 0.05),
    t.0.8 = c(1.2, 1.0, 0.9, 0.2),
    t.0.9 = c(1.3, 1.1, 1.0, 0.3),
    t.0.95 = c(1.4, 1.2, 1.1, 0.4),
    t.0.975 = c(1.5, 1.3, 1.2, 0.5)
  )
  
  expect_message(
    tipmap_plot(tipmap_data, null_effect = 0),
    "95% credible interval for MAP prior includes null treatment effect"
  )
})

test_that("tipmap_plot reports target population without borrowing", {
  tipmap_data <- data.frame(
    x.at = c(-0.15, 0.2, 0.4, 1.15),
    x.col = factor(c("new.obs", "post", "post", "prior")),
    t.est = c(1.0, 0.8, 0.6, 0.1),
    t.0.025 = c(0.2, 0.2, 0.1, -0.2),
    t.0.05 = c(0.3, 0.3, 0.2, -0.1),
    t.0.1 = c(0.4, 0.4, 0.3, 0.0),
    t.0.2 = c(0.5, 0.5, 0.4, 0.05),
    t.0.8 = c(1.2, 1.0, 0.9, 0.2),
    t.0.9 = c(1.3, 1.1, 1.0, 0.3),
    t.0.95 = c(1.4, 1.2, 1.1, 0.4),
    t.0.975 = c(1.5, 1.3, 1.2, 0.5)
  )
  
  expect_message(
    tipmap_plot(tipmap_data, null_effect = 0),
    "Treatment effect in target population without borrowing"
  )
})
