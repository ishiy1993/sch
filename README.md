# A CLI client of Google Calendar
## Features

+ Create a event
+ Check events in a given time period

## Install and Setup

0. Enable Google Calendar API and Get `client_id`, `client_secret`
0. Set `client_id`, `client_secret` to environment variables `SCH_CLIENT_ID`, `SCH_CLIENT_SECRET`
0. Clone this repo, then `stack install`

## Usage
Create a event

```
$ sch new "Go shopping" 2017-02-01 -f 13:00 -t 15:00
```

Check events

```
$ sch ls today
$ sch ls thisweek
$ sch ls 2017-02-01
$ sch ls 2017-02-01 2017-03-01
```
