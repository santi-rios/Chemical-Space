To perform a load test you’ll need two pieces of software:

    - shinyloadtest is an R package used to generate recordings and analyze results. You should install it on your development machine with install.packages("shinyloadtest").
    - shinycannon is a command-line used to replay recordings in parallel.

Then run shinycannon from the terminal with a command like this:

```bash
shinycannon recording_dplyr.log http://127.0.0.1:7365 \
  --workers 10 \
  --loaded-duration-minutes 5 \
  --output-dir test/replay2
```

There are six arguments to shinycannon:

    The first argument is a path to the recording that you created in the previous step.

    The second argument is the url to your Shiny app (which you copied and pasted in the previous step).

    --workers sets the number of parallel users to simulate. The above command will simulate the performance of your app as if 10 people were using it simultaneously.

    --loaded-duration-minutes determines how long to run the test for. If this is longer than your script takes, shinycannon will start the script again from the beginning.

    --output-dir gives the name of the directory to save the output. You’re likely to run the load test multiple times as you experiment with performance improvements, so strive to give informative names to these directories.
