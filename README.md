# The BigPanda Backend Exercise challenge

Your task is to implement a Non Blocking Producer/Consumer stream processing service that exposes an HTTP API.

You are provided with a blackbox executable that spits out an __infinite__ stream of lines of event data encoded in JSON. You can download it:
* Linux - https://s3-us-west-1.amazonaws.com/bp-interview-artifacts/generator-linux-amd64
* Mac OS X - https://s3-us-west-1.amazonaws.com/bp-interview-artifacts/generator-macosx-amd64
* Windows - https://s3-us-west-1.amazonaws.com/bp-interview-artifacts/generator-windows-amd64.exe

Run it, you should see something like this:
```bash
➜  backend-challenge git:(master) ✗ json-generator/generator-macosx-amd64
```
```json
{ "event_type": "baz", "data": "amet", "timestamp": 1564039783 }
{ "event_type": "foo", "data": "lorem", "timestamp": 1564039789 }
{ "event_type": "baz", "data": "ipsum", "timestamp": 1564039789 }
{ "event_type": "foo", "data": "dolor ipsum", "timestamp": 1564039789 }
```
```
{ "Cu*S
{ "5Scs\*
{ "XNq
```
```json
{ "event_type": "foo", "data": "lorem", "timestamp": 1564039789 }
```

Service Requirements

* It should consume the output of the generator and gather the following stats:
- A count of events by event type
```json
{ "foo": 3, "baz": 2 }
```
- A count of words appearances, found in each event in the data field of the events
```json
{ "amet": 1, "lorem": 2, "dolor": 1, "ipsum": 2 }
```
- It should expose these stats in an HTTP interface.
- Stream may encounter corrupt JSON lines and should handle such events well and without interruption.

# HTTP API Spec

[HTTP API SPECS](https://bigpandaio.github.io/challenge/index.html)

# Important Notes 

*_Please read thoroughly as it may affect your submission!_*

* We are looking for simple readable code which is not over-engineered.
* The design of your solution should decouple the reads from the writes. Try to think on what it means when scaling such a service.
* You can implement this exercise in any language.
* If you already know some reactive framework (*For example:* RXJava, RxScala, Play, Akka, VertX, Reactor or anything similar) use what you know! if you don't know any of these frameworks read a bit and understand what your implementation can benefit from these proposed frameworks.
* (Optional) For bonus points: Submit using a purely functional language (Scala/Haskell/Purescript/...)
* Add a README file  with instructions on running the project. In the README file, please note *3 things* you would improve in your submission.

# Task Submission

- Fork this repo
- Submit a PR against this repo
- Once you done send a mail to [challenge@bigpanda.io](mailto:challenge@bigpanda.io).
<br/>OR
- If you need to be discrete send your solution directly to [challenge@bigpanda.io](mailto:challenge@bigpanda.io).