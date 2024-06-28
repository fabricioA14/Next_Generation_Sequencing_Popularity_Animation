# Next_Generation_Sequencing_Popularity_Animation
Next-generation sequencing (NGS) platforms, like Illumina, PacBio, and Oxford Nanopore, are revolutionizing genomics. Let’s visualize their popularity and stock performance over different time scales using R.

Cooking Animations in R: Visualizing Trends and Stock Prices

Install and load necessary packages: tidyquant, gtrendsR, ggplot2, gganimate, and more.

Fetch and process Yahoo Trends data: Define companies, set date ranges, and normalize hits over a 10-year period.

Smooth and visualize trends: Compute smoothed data with standard errors and create a dynamic ggplot showing popularity trends.

Fetch and process stock prices: Get data for Illumina, PacBio, and Oxford Nanopore, aggregate it, and prepare for visualization of post-pandemic trends.

Create animated stock price plot: Customize ggplot to show adjusted stock prices over time with dynamic titles and subtitles.

Merge animations: Combine the trend and stock price plots into a single animated GIF for a comprehensive view.

## Directory Structure

The project is organized as follows:

- **/src**: Directory containing scripts and dependencies.  
  - `dependencies.R`: Contains the script for installing and loading the necessary R packages and custom function.
- `sequencing_platforms_animation.R`: Main script that calls the functions and executes the project workflow.

# Animation

After running `sequencing_platforms_animation.R`, the animation result is saved as a GIF file. Below is the animation:

![combined_gif](https://github.com/fabricioA14/Next_Generation_Sequencing_Popularity_Animation/assets/73892283/a17fb321-1978-4df7-8a91-b3fc57595410)
