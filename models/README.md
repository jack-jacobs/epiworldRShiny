# Models included in the app

This folder contains the models used in the app. Models have the following two functions: `shiny_model` and `model_panel`. The `shiny_model` function is used to run the model and return the results. The `model_panel` function is used to create the model's panel in the app.

If you are adding a new model, you will need to add a new file to this folder. The UI and server will capture it automatically. The accompanying `shiny_model.md` file will be used to generate the model's documentation in the app.
