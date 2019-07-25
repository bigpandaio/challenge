# The Bigpanda Sr.DevOps Exercise challenge

Your task is to implement a CI/CD pipeline for an existing open source project of your choice.

**Prerequisites** | **Tools / Online account(s) needed to perform this exercise**

* Github / Other online git service account
* CI server / online service (e.g Jenkins/Travis) - build based on code change
* Docker container knowledge - containerize your app
* Configuration Management tool (ansible or other) - install system

**Requirements**

- Deployment tool - deploy latest code
- Vagrant | Virtualbox | AWS (free tier account) | Online solution

# Objectives

- Setup a CI flow for an open source project - it can be in any program language, but must have some level of unit testing (we can provide an example if needed).
- Build & Deploy a service as a Docker container activity

# High level guidance

- Create / Clone an existing project on Github - please choose an existing open source project which doesn’t have a Docker distro already, alternatively improve an existing one (and explain why in your project's README.md)
- Containerize your service
- Setup or Use CI-service (e.g travis-ci/jenkins)
- Supply a deploy directory which includes some configuration management tool.
- Deployment tool - which deploys your docker container on a docker enabled host.
- Compose a succinct README.md which will enable any customer to follow a step
by step explanation from the `git clone` until the `running sample`.
- In the README file, please note **3 things** you would improve in your submission.

# Clarifications

- Deliverable will be in a form of a PR (1 or more) + access to CI process/server

- Any manual instructions should be supplied as part of the PR in a form of README.md

- For any questions you can contact us back

# Task Submission

- Fork this repo
- Submit a PR