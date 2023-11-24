# Tests for UI Helpers functions

# collapseInstructions ----

test_that("collapseInstructions() stops on invalid nmsp arg.", {
    ns <- NS('test')
    out <- collapseInstructions(nmsp = ns, id = 'test-id',
        ttl = '<Open Instructions>',icon = 'circle-info',
        p('my html')
    )
    expect_equal(as.character(out), "<div class=\"row\">\n  <p>\n    <i class=\"fa-solid fa-circle-info\"></i>\n    <a data-bs-toggle=\"collapse\" href=\"#test-test-id\" role=\"button\" aria-expanded=\"false\" aria-controls=\"test-test-id\">&lt;Open Instructions&gt;</a>\n  </p>\n  <div class=\"collapse\" id=\"test-test-id\">\n    <p>my html</p>\n  </div>\n</div>")

    ns <- 'test'
    expect_error(
        collapseInstructions(nmsp = ns, id = 'test-id',
            ttl = '<Open Instructions>',icon = 'circle-info',
            p('my html')
        ),
        'Namespace argument `nmsp` is not a function'
    )
 })
