# Cowboy Riak Core VNode Dispatcher

This is a port of Jon Brisbin's [Misultin Riak Core VNode dispatcher example](https://github.com/jbrisbin/misultin-riak-core-vnode-dispatcher) to [Cowboy](http://github.com/extend/cowboy).

Misultin development has been discontinued and Cowboy seems to be HTTP server *du jour* in the Erlang
community, so usage of Cowboy is encouraged.


### How do I use it?

To test out this code, build it by typing

    rebar get-deps && make

then start a node with

    ./node1.sh

Send a web request using curl or somesuch:

    curl -v http://localhost:3000/

You should get "Hello World!" back and the `X-Handling-Node` header will return the name of the vnode
that handled the request:

    X-Handling-Node: helloworld2@localhost

To test the clustering capabilities, open a second console window and start another node using this `./node2.sh` 
script. Once started, the two nodes should be clustered together and in a riak_core ring together. Send 
another request like above to make sure both instances can respond.

You know have a clustered web application! If you kill one of the processes, your application will continue 
to function (provided you send HTTP traffic to the other node, of course... this is where an 
haproxy or other load-balancer would come in).
