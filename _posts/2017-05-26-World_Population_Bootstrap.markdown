---
layout:     post
title:      World Population Bootstrap
author:     Brian Jo
tags:       post template
subtitle:   Trying out first post
category:   test
---
<!-- Start Writing Below in Markdown -->

Imagine this scenario: an alien that has stumbled upon Earth learns that
humans are roughly organized into political entities called countries.
These countries are separated by geographic boundaries, means of
communication and cultures, and without any cooperation from the local
authorities, it wants to estimate the population of each country. If the
alien has the means to truly randomly draw people across different
regions (such that each draw is independent), and also be able to
determine which country the individual is from, how confident would it
be in its estimate of each country's population?

One good way to go about approaching this problem is through a
statistical method called
[bootstrapping](https://en.wikipedia.org/wiki/Bootstrapping_(statistics)).
Bootstrapping refers to a nonparametric statistical method that relies
on resampling of the original data. Bootstrapping is attributed to
[Efron](http://projecteuclid.org/euclid.aos/1176344552), who extended
the [jackknife](https://en.wikipedia.org/wiki/Jackknife_resampling)
method as a way to estimate a statistic of interest by re-drawing the
data points from the empirical probability distribution (the given data
distribution). One of the biggest benefits of bootstrapping is that it
provides a very simple, fast and intuitive way to generate confidence
intervals on a statistic that we are trying to estimate.

Let's get right to the problem that we are faced with: estimating the
population of each country on Earth from a limited sampling of humans.

First, we download the data of world population by country from the
[World Bank](http://data.worldbank.org/indicator/SP.POP.TOTL) to sample
from. In particular, let's use the data from year 2014:

{% highlight R %}

    library(ggplot2)
    library(matrixStats)

    # Need to do some data cleaning
    world_pop_data = read.table('API_SP.POP.TOTL_DS2_en_csv_v2/API_SP.POP.TOTL_DS2_en_csv_v2.csv', sep=',', stringsAsFactors=FALSE, header=TRUE)
    world_pop_meta = read.table('API_SP.POP.TOTL_DS2_en_csv_v2/Metadata_Country_API_SP.POP.TOTL_DS2_en_csv_v2.csv', sep=',', stringsAsFactors=FALSE, header=TRUE)
    # Only take countries
    row.names(world_pop_meta) = world_pop_meta$Country.Code
    world_pop_meta = world_pop_meta[(!(world_pop_meta$Region == "")),]

    world_pop_data_filter = world_pop_data
    row.names(world_pop_data_filter) = world_pop_data_filter$Country.Code
    world_pop_data_filter = world_pop_data_filter[(row.names(world_pop_data_filter) %in% row.names(world_pop_meta)),]

    # Take the population in year 2014, and the total as the sum of all rows (countries)
    total_documented_pop = sum(world_pop_data_filter$X2014)
    pop_vector = world_pop_data_filter$X2014
    sample_df = data.frame(CountryCode = row.names(world_pop_data_filter), CountryName = world_pop_data_filter$Country.Name, TotalPop = pop_vector)
    print(head(sample_df, n = 10))

    ##    CountryCode          CountryName TotalPop
    ## 1          ABW                Aruba   103441
    ## 2          AND              Andorra    72786
    ## 3          AFG          Afghanistan 31627506
    ## 4          AGO               Angola 24227524
    ## 5          ALB              Albania  2894475
    ## 6          ARE United Arab Emirates  9086139
    ## 7          ARG            Argentina 42980026
    ## 8          ARM              Armenia  3006154
    ## 9          ASM       American Samoa    55434
    ## 10         ATG  Antigua and Barbuda    90900

    print(dim(sample_df))

    ## [1] 214   3

{% endhighlight %}


There are 214 'countries' in this dataset for which a population is
given. (Actually, not all of the rows are real countries. For example,
the first entry of this table, Aruba, is actually part of the
Netherlands. For a more detailed discussion of what actually is a
country, refer to this nice
[introduction](https://www.youtube.com/watch?v=4AivEQmfPpk). But for the
sake of this exercise, let's assume that each row is a country and it
accurately gives us the population of each country.)

Let's take a look at how the population is distributed among countries:

{% highlight R %}

    ordered_population_df = data.frame(ind = c(1:214), name = sample_df$CountryName[(order(pop_vector, decreasing = TRUE))], pop = pop_vector[(order(pop_vector, decreasing = TRUE))])
    print(head(ordered_population_df, n = 20))

    ##    ind               name        pop
    ## 1    1              China 1364270000
    ## 2    2              India 1295291543
    ## 3    3      United States  318857056
    ## 4    4          Indonesia  254454778
    ## 5    5             Brazil  206077898
    ## 6    6           Pakistan  185044286
    ## 7    7            Nigeria  177475986
    ## 8    8         Bangladesh  159077513
    ## 9    9 Russian Federation  143819569
    ## 10  10              Japan  127131800
    ## 11  11             Mexico  125385833
    ## 12  12        Philippines   99138690
    ## 13  13           Ethiopia   96958732
    ## 14  14            Vietnam   90728900
    ## 15  15   Egypt, Arab Rep.   89579670
    ## 16  16            Germany   80970732
    ## 17  17 Iran, Islamic Rep.   78143644
    ## 18  18             Turkey   75932348
    ## 19  19   Congo, Dem. Rep.   74877030
    ## 20  20           Thailand   67725979

{% endhighlight %}

We have two countries (China and India) whose population exceeds one
billion, followed by nine countries whose population exceeds 100
million. It might be reasonable to guess that the population
distribution by country is roughly log-normal, as this pattern appears
regularly in nature.

{% highlight R %}

    ggplot(data.frame(ind = c(1:214), pop = log10(pop_vector)), aes(pop)) + geom_histogram(bins=50) + ggtitle('Log10 of population histogram') + xlab('Log10 of population')

{% endhighlight %}

![img1](http://projectpages.github.io/project-pages/img/src/stats/world_population_bootstrap/chunk-4-1.png)

However, we can see that the population is not distributed in a
log-normal fashion. There is also another phenomenon, [Zipf's
Law](https://en.wikipedia.org/wiki/Zipf%27s_law), that is frequently
invoked to model various rank-frequency distributions, such as frequency
of words used in a language or population rank of cities. Let's try to
see if the Zipf model is reasonable for population distribution over
countries:

{% highlight R %}

    ordered_population_df$zipf_pop = sapply(c(1:214), function(x) {max(pop_vector)/x})
    ggplot(ordered_population_df, aes(ind)) + geom_line(aes(y = log10(pop), colour = "Actual")) + geom_line(aes(y = log10(zipf_pop), colour = "Theoretical Zipfian")) + ggtitle('Log10 of population by country') + xlab('Country index')

{% endhighlight %}

![img2](http://projectpages.github.io/project-pages/img/src/stats/world_population_bootstrap/chunk-5-1.png)

    ggplot(ordered_population_df[c(1:50),], aes(ind)) + geom_line(aes(y = log10(pop), colour = "Actual")) + geom_line(aes(y = log10(zipf_pop), colour = "Theoretical Zipfian")) + ggtitle('Log10 of population by country') + xlab('Country index')

{% endhighlight %}

![img3](http://projectpages.github.io/project-pages/img/src/stats/world_population_bootstrap/chunk-5-2.png)

We can see that while this model fits remarkably well for the first 50
countries or so, it falls apart after that: it seems like there is
another phenomenon that govern countries that have smaller populations.

Since we can't make any conclusive statements about the true
distribution, (and since our alien friend actually doesn't have access
to this information), let's get back to our sampling (alien abduction)
scheme. What if our capacity to sample has a maximum of 100,000
individuals? Would that give us a pretty good estimate for most
countries?

{% highlight R %}

    # Let's reorder the sample_df and pop_vector
    sample_df = sample_df[(order(pop_vector, decreasing = TRUE)),]
    row.names(sample_df) = sample_df$CountryCode
    pop_vector = pop_vector[(order(pop_vector, decreasing = TRUE))]

    # A sample from world population - picking 100,000 people
    pop_vector_prob = pop_vector/total_documented_pop
    set.seed(111)
    temp_sample = sample(row.names(world_pop_data_filter), size=100000, replace = TRUE, prob = pop_vector_prob)

    sample_df$SamplePop = sapply(row.names(world_pop_data_filter), function(x) {sum(temp_sample == x)})
    print(head(sample_df, n = 10))

    ##     CountryCode        CountryName   TotalPop SamplePop
    ## CHN         CHN              China 1364270000     18983
    ## IND         IND              India 1295291543     17876
    ## USA         USA      United States  318857056      4448
    ## IDN         IDN          Indonesia  254454778      3555
    ## BRA         BRA             Brazil  206077898      2820
    ## PAK         PAK           Pakistan  185044286      2483
    ## NGA         NGA            Nigeria  177475986      2428
    ## BGD         BGD         Bangladesh  159077513      2162
    ## RUS         RUS Russian Federation  143819569      2004
    ## JPN         JPN              Japan  127131800      1706

    print(tail(sample_df, n = 20))

    ##     CountryCode               CountryName TotalPop SamplePop
    ## ATG         ATG       Antigua and Barbuda    90900         1
    ## IMN         IMN               Isle of Man    87127         2
    ## AND         AND                   Andorra    72786         1
    ## DMA         DMA                  Dominica    72341         0
    ## BMU         BMU                   Bermuda    65181         0
    ## CYM         CYM            Cayman Islands    59172         2
    ## GRL         GRL                 Greenland    56295         0
    ## ASM         ASM            American Samoa    55434         0
    ## KNA         KNA       St. Kitts and Nevis    54944         0
    ## MNP         MNP  Northern Mariana Islands    54541         1
    ## MHL         MHL          Marshall Islands    52898         0
    ## FRO         FRO             Faroe Islands    48221         3
    ## SXM         SXM Sint Maarten (Dutch part)    37685         0
    ## MCO         MCO                    Monaco    37623         1
    ## LIE         LIE             Liechtenstein    37286         0
    ## TCA         TCA  Turks and Caicos Islands    33740         0
    ## SMR         SMR                San Marino    31595         2
    ## MAF         MAF  St. Martin (French part)    31530         1
    ## PLW         PLW                     Palau    21097         0
    ## TUV         TUV                    Tuvalu     9893         0

{% endhighlight %}

As we can see from above, roughly 19% of our samples are Chinese, while
roughly 18% are Indian. But there actually are a lot countries (15 to be
exact in this example), that are missing in this sample. We will return
to this issue of unrepresented countries later. For now, we don't know
that these countries exist because they are not present in our sample.
Let's try to see how well our sample corresponds to the underlying
reality, with 'Actual' distribution scaled by our sample size:

{% highlight R %}

    sample_df$ind = c(1:214)
    diff = log10(total_documented_pop/100000)
    # We're missing representatives from 15 countries in this sample.
    ggplot(sample_df, aes(ind)) + geom_line(aes(y = log10(TotalPop)-diff, colour = "Actual")) + geom_line(aes(y = log10(SamplePop), colour = "Sample")) + ggtitle('Log10 of population by country, sampled') + xlab('Country index') + ylab('Sampled population')

{% endhighlight %}

![img4](http://projectpages.github.io/project-pages/img/src/stats/world_population_bootstrap/chunk-7-1.png)

It seems pretty good right now. Now that we have the 100,000
individuals, we can politely ask each person what their nationality is,
and then we can calculate the proportion of each country represented.
This will give us [*a point
estimate*](https://en.wikipedia.org/wiki/Point_estimation) of each
country's proportion of total. And if we knew the total population, this
proportion can be translated directly to an actual population point
estimate.

For example, since we have 18,983 Chinese individuals, our estimate,
given that we know the world total, would be ~1373700000, which is
pretty close to 1364300000 in the real data (about 0.7% error).

However, these simple point estimates are a bit problematic: they don't
encode any uncertainty in our sampling distribution, and hence they
don't give us any information about how confident we can be about these
estimates. For example, given two individuals from San Marino, we have
no idea what to expect in terms of how much the actual population can
vary *around* 0.002% of the total, which is our point estimate. Indeed,
the point estimate from two individuals in this case (if we knew the
total population, which is actually not the case) would be 144726, and
this value is quite different from the actual population of 31595.

So this is where bootstrapping comes in. The underlying idea is simple -
we can provide a measure of uncertainty in the underlying distribution
by re-sampling from our data, and seeing how our statistic (population
proportions in this case) fluctuates. Let's start with 100 re-samplings:

{% highlight R %}

    # Let's generate the bootstrap samples:
    pop_code_vector = unlist(sapply(c(1:214), function(x) {rep(as.character(sample_df$CountryCode[x]), sample_df$SamplePop[x])}))

    # Statistic to save: the bootstrap sample of each country, as well as the number of unique countries:
    bootstrap_reps = 100
    num_countries = rep(0, bootstrap_reps)
    bootstrap_samples = matrix(0,214,bootstrap_reps)

    # The following code was run for the bootstrap sample generation. I saved the result to save time during the writeup:
    #for (i in c(1:bootstrap_reps)) {
    #  print(i)
    #  bts_sample = pop_code_vector[sample(c(1:100000), size = 100000, replace = TRUE)]
    #  num_countries[i] = length(unique(bts_sample))
    #  bootstrap_samples[,i] = sapply(c(1:214), function(x) {sum(bts_sample == sample_df$CountryCode[x])})
    #}
    #save(num_countries, bootstrap_samples, file = 'bootstrap_results_100.RData')

    load('bootstrap_results_100.RData')

{% endhighlight %}

Now that we have the bootstrapped samples, there are actually several
ways we can assign confidence intervals in our estimated statistic.
Here, I'll go over three methods:

1.  Normal Interval

The premise of the normal interval is simple: we asuume that the
statistic estimated from our bootstrap samples has a roughly Gaussian
distribution. Therefore, we can assign a confidence interval that
corresponds to our desired level of confidence. For example, if we want
to produce a 95% confidence interval, since 95% of samples in a Gaussian
distribution fall within ~1.96 \* se (standard error) of the mean, we
can say that this interval around the boostrap mean constitutes our
confidence interval.

1.  Percentile Interval

The percentile interval is also very intuitive: if we want to have a 95%
confidence interval, we simply take the statistic that corresponds to
2.5% and 97.5% percentile of our bootstrap samples, and assign them to
define our confidence interval.

1.  Pivotal Interval

The pivotal interval is of a slightly different flavor. Let's say the
true underlying statistic is *θ*, and the estimate of this from our data
is $\\hat{\\theta}\_n$. Then, define the pivot :
$$R\_n = \\hat{\\theta}\_n - \\theta$$
 which measures how far off our estimate is from the true statistic. We
want to find the distribution of this pivot, but since we don't have
access to the true statistic and have only one data sample (our given
data), we need to estimate it from the bootstrap samples:
$$\\hat{R}\_{n,b} = \\hat{\\theta}\_{n,b} - \\hat{\\theta}\_n$$
 where $\\hat{\\theta}\_{n,b}$ is our single bootstrap estimate. After
some algebra, we can show that the estimate of the confidence interval
from this empirical pivot distribution is defined as:

$$ C\_n = (2\\hat{\\theta}\_n - \\hat{\\theta}\_{(1-\\alpha/2)B}, 2\\hat{\\theta}\_n - \\hat{\\theta}\_{(\\alpha/2)B}) $$
 where (1 − *α*/2)*B* and (*α*/2)*B* is similar to the percentile of the
boostrap statistics defined above.

Let's see these confidence intervals in action, and we how well it
performed in our data:

{% highlight R %}

    # Let's generate the confidence interval for the proportion of each country's population:
    rowmean = rowMeans(bootstrap_samples)
    rowvar = rowVars(bootstrap_samples)
    bootstrap_df = sample_df
    bootstrap_df$bts_mean_est = rowmean
    # Normal interval
    bootstrap_df$bts_norm_conf_low = rowmean - 1.96*sqrt(rowvar)
    bootstrap_df$bts_norm_conf_high = rowmean + 1.96*sqrt(rowvar)
    # Of course, the population can't be negative:
    bootstrap_df$bts_norm_conf_low[bootstrap_df$bts_norm_conf_low<0] = 0

    # Percentile interval
    rowquantiles = rowQuantiles(bootstrap_samples, probs=c(0.025, 0.975))
    bootstrap_df$bts_percent_conf_low = rowquantiles[,1]
    bootstrap_df$bts_percent_conf_high = rowquantiles[,2]

    # Pivotal confidence interval
    bootstrap_df$bts_pivotal_conf_low = 2 * bootstrap_df$SamplePop - rowquantiles[,2]
    bootstrap_df$bts_pivotal_conf_high = 2 * bootstrap_df$SamplePop - rowquantiles[,1]
    # Of course, the population can't be negative:
    bootstrap_df$bts_pivotal_conf_low[bootstrap_df$bts_pivotal_conf_low<0] = 0

    ggplot(bootstrap_df[c(1:50),], aes(ind, log10(TotalPop)-diff))+geom_point()+geom_line(aes(ind,log10(bts_mean_est)))+geom_ribbon(aes(ymin=log10(bts_norm_conf_low),ymax=log10(bts_norm_conf_high)),alpha=0.3) + ggtitle('Countries 1-50, bootstrap estimates of population')

{% endhighlight %}

![img5](http://projectpages.github.io/project-pages/img/src/stats/world_population_bootstrap/chunk-9-1.png)

{% highlight R %}

    ggplot(bootstrap_df[c(51:150),], aes(ind, log10(TotalPop)-diff))+geom_point()+geom_line(aes(ind,log10(bts_mean_est)))+geom_ribbon(aes(ymin=log10(bts_norm_conf_low),ymax=log10(bts_norm_conf_high)),alpha=0.3) + ggtitle('Countries 51-150, bootstrap estimates of population')

{% endhighlight %}

![img6](http://projectpages.github.io/project-pages/img/src/stats/world_population_bootstrap/chunk-9-2.png)

{% highlight R %}

    ggplot(bootstrap_df[c(151:214),], aes(ind, log10(TotalPop)-diff))+geom_point()+geom_line(aes(ind,log10(bts_mean_est)))+geom_ribbon(aes(ymin=log10(bts_norm_conf_low),ymax=log10(bts_norm_conf_high)),alpha=0.3) + ggtitle('Countries 151-214, bootstrap estimates of population')

{% endhighlight %}

![img7](http://projectpages.github.io/project-pages/img/src/stats/world_population_bootstrap/chunk-9-3.png)
