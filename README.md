![Data61 Logo](https://raw.githubusercontent.com/qfpl/assets/master/data61-transparent-bg.png)

This library provides `reflex` support for writing TCP servers and
clients. All the socket operations are wrapped to use Reflex `Event`s,
and the underlying sending/receiving/waiting/accepting operations are
performed on background threads.

The most important function in this library is
`Reflex.Backend.Socket.socket`, which wraps a `Socket` to process
`Event t ByteString`s.

That `Socket` can come from:

1. `Reflex.Backend.Socket.Accept.accept`, if you're making a server;
2. `Reflex.Backend.Socket.Connect.connect`, if you're making a client; or
3. Your favourite networking library.

Check the `example` subdirectory for some examples. Example clients
and servers are provided.
