# Continuous development, continuous integration

To avoid unnecessary power and resource consumption, we have a tiered CI. On
pull requests, we provide fast feedback. In the merge queue and when pushing to
`main` we run a larger set of jobs to ensure we test extensively and to make
sure we get good caching behaviour. Finally we have nightly CI that runs a
complementary set of jobs on the `main` branch.

## Nightly CI

Nightly CI runs jobs that are important in their own right, but that are either
too costly or disruptive to run as part of the regular development cycle (PR,
merge queue, `main` branch). These  workflows/jobs are run as part of nightly
CI:

* The [Check Links][check-links] only runs nightly at 18:00 UTC.
* The [Examples][examples] workflow runs nightly at 18:30 UTC with a
  comprehensive set of examples. In the regular development cycle, only a small
  subset of these examples is run.

GitHub Actions does not support enabling notifications for scheduled workflows,
but users can subscribe to notifications from issues. So if any jobs in a
nightly scheduled workflow fail, then the results will be reported as a comment
on an issue. Anyone who is subscribed to such an issue will get a notification
when a comment is created with a report. There is one issue for each nightly
scheduled workflow.

* For nightly runs of the [Check Links][check-links] workflow, results will be
  posted on [issue #1972](https://github.com/well-typed/hs-bindgen/issues/1972).
* For nightly runs of the [Examples][examples] workflow, results will be posted
  on [issue #1977](https://github.com/well-typed/hs-bindgen/issues/1977).

<!-- sources and references -->

[check-links]: https://github.com/well-typed/hs-bindgen/actions/workflows/check-links.yml
[examples]: https://github.com/well-typed/hs-bindgen/actions/workflows/examples.yml
