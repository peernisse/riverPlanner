# TESTS FOR helpers_trip

# alertIcon()

test_that('alertIcon() returns valid HTML',{

    out <- alertIcon(icon = 'info', type = 'primary',
        style = 'margin: 10px; border-left-width: 10px;',
        dismissible = TRUE,
        p(style = 'margin-top: 15px;', 'TEST'
        )
    )

    expect_equal(as.character(out),
                 "<div class=\"alert alert-primary alert-dismissible d-flex align-items-center\" role=\"alert\" style=\"margin: 10px; border-left-width: 10px;\">\n  <button type=\"button\" class=\"btn-close\" style=\"display: ;\" data-bs-dismiss=\"alert\"></button>\n  <svg xmlns=\"http://www.w3.org/2000/svg\" width=\"24\" height=\"24\" fill=\"currentColor\" class=\"bi bi-M8 16A8 8 0 1 0 8 0a8 8 0 0 0 0 16zm.93-9.412-1 4.705c-.07.34.029.533.304.533.194 0 .487-.07.686-.246l-.088.416c-.287.346-.92.598-1.465.598-.703 0-1.002-.422-.808-1.319l.738-3.468c.064-.293.006-.399-.287-.47l-.451-.081.082-.381 2.29-.287zM8 5.5a1 1 0 1 1 0-2 1 1 0 0 1 0 2z-fill flex-shrink-0 me-2\" viewbox=\"0 0 16 16\" role=\"img\" aria-label=\"Warning:\">\n    <path d=\"M8 16A8 8 0 1 0 8 0a8 8 0 0 0 0 16zm.93-9.412-1 4.705c-.07.34.029.533.304.533.194 0 .487-.07.686-.246l-.088.416c-.287.346-.92.598-1.465.598-.703 0-1.002-.422-.808-1.319l.738-3.468c.064-.293.006-.399-.287-.47l-.451-.081.082-.381 2.29-.287zM8 5.5a1 1 0 1 1 0-2 1 1 0 0 1 0 2z\"></path>\n  </svg>\n  <div>\n    <p style=\"margin-top: 15px;\">TEST</p>\n  </div>\n</div>"
    )

})
