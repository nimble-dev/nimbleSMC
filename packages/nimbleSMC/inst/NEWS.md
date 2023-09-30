#                        CHANGES IN VERSION 0.11.0 (September 2023)

This release allows `nimbleSMC` to work with `nimble` versions 1.0.0 and later (i.e., AD-enabled NIMBLE).

## USER LEVEL CHANGES

- Improve efficiency of ensemble Kalman Filter (issue #2).

- Check and error out when detect deterministic latent nodes (issue #5).

## DEVELOPER LEVEL CHANGES

- Update canned action versions in GitHub Actions continuous integration.
                        
#                        CHANGES IN VERSION 0.10.1 (December 2021)

## BUG FIXES

- Ensure that IF2 correctly handles switchover from run to continueRun given C++ increments loop counter at conclusion of loop (see nimble repo issue 1184).

## DEVELOPER LEVEL CHANGES

- Avoid numerical problems from underflow caused by exponentiating weights in various filters (PR #4).

- Switch citation to newly-published JSS paper.

- Make various cleanups to testing.

- Move continuous integration testing to GitHub Actions.

#                        CHANGES IN VERSION 0.10.0 (October 2020)

Sequential Monte Carlo functionality was moved from the core nimble package as of nimble version 0.10.0.
