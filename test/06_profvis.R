library(profvis)

profvis({ shiny::runApp() }
        # , prof_output = './test/profvis/'
        )