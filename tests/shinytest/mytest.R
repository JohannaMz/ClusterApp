app <- ShinyDriver$new("../../", loadTimeout = 1e+05)
app$snapshotInit("mytest")

app$setInputs(demo_data = "Demo data wolf")
app$snapshot()
