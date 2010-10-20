# Erlang Webstuff

This repository is built to contain a large amount of "Web Stuff" for
Erlang. The "Web Stuff" is a set of RFCs and other "enterprisey"
oriented protocols. The general rule for these are that they are
time-consuming to implement from scratch and so provides a barrier to
entry for Erlang programs in companies and projects.

## Status

*WE RECOMMEND CARE* when using the library in its current state. More
work is needed.

### TODO List

   * Introduce and write down test cases
   * Document conventions used
   * Think about when and how to freeze a library


   * Introduce an (un-)parser for ISO8601 / RFC3339

## Contents

   * uri.erl -- Manipulation of URIs (RFC3986)
   * iri.erl -- Manipulation of IRIs (RFC3987)
   * inet_time.erl -- "Internet time" manipulation (RFC3339)

## Goal

The purpose here is to attempt an implementation. If it succeeds, it
means that Erlang programmers will be able to stand on the shoulders
of the code and reach up farther and faster.

## Methodology

One Erlang Mantra is "Provide Tools, not solutions". So rather than
providing a full implementation with a pre-defined way to interface
with it, we will try to provide a low-level set of operations which
are mere work to implement yourself. If you need a way to do a
specific encoding of, say, URIs you should be able to find that in
this library along with the usual operations for manipulation.

In addition to providing the toolbox, we have a mantra that each
library here should be dogfooded. We prefer a real-world practical
application as an API test - because it tends to weed out bad design
decisions easily.