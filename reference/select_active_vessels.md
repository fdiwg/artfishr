# Select active vessels

Select active vessels for a selection of years

**Active vessels**:

The `active_vessels` can be either time-dependent,ie given by
`year`/`month`, `year` only or be atemporal, mainly depending on how and
at which frequency the vessels census/survey is operated in the country.

An additional argument called `active_vessels_strategy` controls how to
select the `active_vessels` data in time, when this data is
time-dependent. The `latest` strategy will select the latest data
acquired in time, while the `closest` will select the closest data
acquired in time, which could be data acquired after the effort data
considered.

As example, a vessel census is performed each five years, and data is
available for 2007 and 2012. Effort data for 2011 needs to be computed.
Which `active_vessels` data should be used? In case of a `latest`
strategy, in 2011, the latest data available is 2007, so this one will
be used. In the case of a `closest` strategy, in 2011, we used the
closest vessel data (ie 2012), assuming it betters characterizes the
fleet engagement for the year considered.

## Usage

``` r
select_active_vessels(
  periods,
  active_vessels,
  active_vessels_strategy = c("latest", "closest")
)
```

## Arguments

- periods:

  periods, list of unique years or year/month periods

- active_vessels:

  active vessels object of class tibble

- active_vessels_strategy:

  The strategy to associate the active vessels to the effort based on
  time. Active vessels period does not match necessarily the periods of
  data (effort, landings), and can be reported either by year or by
  year/month. This parameter let decide which methodology should be used
  to select the active vessels based on time. It can be either "latest"
  (taking the latest period), "closest" ie the closest active vessels in
  time, after or before the data period. In case 2 periods before/after
  are equally closer, the latest in time before the data period will be
  taken.

## Value

a series of active vessels selection for the years considered (added as
`ref_period`)
