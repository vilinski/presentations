---
theme : "night"
transition: "slide"
highlightTheme: "monokai"
logoImg: "logo.png"
slideNumber: false
title: "VSCode Reveal intro"
---

::: block
*here be dragons* {style=background:red;width:500px}
:::

---

// @[vine](etVpwB7uHlw)

---

### Solar System Exploration, 1950s – 1960s

- [ ] Mercury
- [x] Venus
- [x] Earth (Orbit/Moon)
- [x] Mars
- [ ] Jupiter
- [ ] Saturn
- [ ] Uranus
- [ ] Neptune
- [ ] Comet Haley
- [ ]

---

# title : Performance Tests
description : Performance Issues, tools, solutions
author : Andreas Vilinski
published : false
slideNumber: true
theme : league
transition: none
revealjs.verticalSeparator: ^( ?| )***( ?| )$
---

# Performance Tests

***

AGENDA

- [title : Performance Tests](#title--performance-tests)
  - [revealjs.verticalSeparator: ^( ?| )***( ?| )$](#revealjsverticalseparator-----)
- [Performance Tests](#performance-tests)
- [Requirements](#requirements)
- [Current Situation](#current-situation)
  - [Other possible problems](#other-possible-problems)
  - [Cause](#cause)
- [Possible Solutions](#possible-solutions)
  - [Improve Process Data throughput:](#improve-process-data-throughput)
  - [Improve Max Topology Size](#improve-max-topology-size)
- [Tools](#tools)
  - [Monitoring tools](#monitoring-tools)
  - [Performance test tools](#performance-test-tools)
  - [Performance test tools in .NET](#performance-test-tools-in-net)
  - [NBomber](#nbomber)
  - [load-test-tool](#load-test-tool)
  - [What's Currently tested](#whats-currently-tested)
  - [What' the problems by testing](#what-the-problems-by-testing)
  - [Whats's not yet tested](#whatss-not-yet-tested)

---

# Requirements

- Process Data flow 2500 measurements/s
- Nodes in the topology 100.000
- [A missing link to Wiki]

---

# Current Situation

- Max 1000 msg/s throughput is possible
- 500 nodes makes problems in UI and DB
- 2-30 rps in http endpoints
- max 30 users
- not scalable microservices

---

## Other possible problems

- distributed endless loops
  - wrong business logic
  - not expected flow in case of errors, timeouts, retries, polling
- not enough isolation - moneo is not responsible because of DB problem in one service

---

## Cause

- no culture of quality code
  - no code reviews
  - no unit tests
  - no integration tests
  - no benchmarks
  - brainless logging
  - ignoring compiler warnings and IDE highlights

---

- code quality check was not setup from begin
- and still not working for now
  - SonarQube
    - free license is only for one branch
    - bad IDE integration
  - JetBrains IDEs are not used enough (coincidentally in bad gitlab repos)

---

- not optimal architecture decisions
  - MessageBus is not an answer for clustered services
  - EntityFramework and client side queries
  - Dependency Injection
- POs are concentrated for feature delivering
  - but perf is also a feature?

---

# Possible Solutions

## Improve Process Data throughput:

- right backends - time series db, message queue
- don't call it "fancy"
- right JSON serializer
- reduce payload - token
- parallelism, batching, reactive streams

research and measure (benchmarks)

---

## Improve Max Topology Size

- check EF-produced DB queries
- switch to SQL if needed - e.g. topology deep search
- check indexes and their usage (postgres query stats)
- eliminate client side queries `ctx.Select(...).AsEnumerable()...`
- eliminate `LIKE %` and other string match where possible
- eliminate `select *` - timeout, memory overflow
- benchmark/integration test for queries (should be team intern)

---

# Tools

## Monitoring tools

- Grafana - graphical insight in processes
- with backend in InfluxDB or Prometheus
- there are alternatives - InfluxDB 2, VictoriaMetrics

---

## Performance test tools

- wrk - simple for simple http tests, lua plugins
- k6 - http (only) tests in pure JS, run compiled in GO
- JMX - java/xml bloatware
- Gattling - powerful, but scala and limited for non http tests

---

## Performance test tools in .NET

- RabbitMQ Firehose
- JetBrains memory and performance profiler - dotTrace, dotMemory
- load-test-tool - building block for performance tests
- NBomber - scenarios in C#/F#

---

## NBomber

- .NET Core - write tests in C#/F#
- fully generic - call any .NET functions, test any drivers or methods
- Scenarios/Steps
- Reports in html/md/txt/csv
- easy Plugins to write other reporters - Influxdb, etc.

---

## load-test-tool

- set of functions to write tests
- generate process data load
- call some of http moneo services
- client for some of http apis

---

## What's Currently tested

- a few test for http
- port from k6 to NBomber
- run-and-observe tests (CPU, Memory, Rabbit messages)
  - generate process data
  - create topology nodes
  - produce threshold violations

## What' the problems by testing

- how to do assertions
  - process data ok, but queue is growing, cpu too high, etc.
  - http calls are ok, but process data doesn't run
- how to setup reference instance
  - create devices, tickets, topology
  - cleanup dbs and queues after test
  - wait until queues and data is gone
- no dedicated infrastructure
  - IPC-10 is shared with Nishad
  - vilinski-test VM, used for outgoing tests
  - moneo instance for each plattform needed
  - strong test PC is coming

## Whats's not yet tested



---

Built with pandoc

    pandoc -t revealjs -s -o slides.html slides.md


meetings einladung - torsten ott
