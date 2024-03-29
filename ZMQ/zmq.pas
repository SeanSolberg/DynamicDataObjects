{
    Copyright (c) 2012 Varga Bal�zs (bb.varga@gmail.com)
    This file is part of 0MQ Delphi binding
    0MQ Delphi binding is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public License as
    published by the Free Software Foundation; either version 3 of the
    License, or (at your option) any later version.
    0MQ Delphi binding is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.
    You should have received a copy of the GNU Lesser General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
    Sean solberg, 10/16/2015.   Adjusted to fit ZMQ 4.1.3.   Not everything is
                                tested, but I really needed the shutdown call.
}
unit zmq;
{$ifdef UNIX}
  {$linklib pthread}
{$endif}
{$I zmq.inc}
interface
{$ifdef FPC}
uses
  ctypes;
{$endif}
const
  {$ifdef UNIX}
  libzmq = 'libzmq.so';
  {$else}
//  libzmq = 'libzmq.dll';
  libzmq = 'libzmq-v120-mt-4_0_4.dll';
  {$endif}
{  Run-time API version detection                                              }
procedure zmq_version( var major, minor, patch: Integer ); cdecl; external libzmq;
{******************************************************************************}
{*  0MQ errors.                                                               *}
{******************************************************************************}
{*  A number random enough not to collide with different errno ranges on      *}
{*  different OSes. The assumption is that error_t is at least 32-bit type.   *}
const
  ZMQ_HAUSNUMERO = 156384712;
{*  On Windows platform some of the standard POSIX errnos are not defined.    *}
  ENOTSUP = (ZMQ_HAUSNUMERO + 1);
  EPROTONOSUPPORT = (ZMQ_HAUSNUMERO + 2);
  ENOBUFS = (ZMQ_HAUSNUMERO + 3);
  ENETDOWN = (ZMQ_HAUSNUMERO + 4);
  EADDRINUSE = (ZMQ_HAUSNUMERO + 5);
  EADDRNOTAVAIL = (ZMQ_HAUSNUMERO + 6);
  ECONNREFUSED = (ZMQ_HAUSNUMERO + 7);
  EINPROGRESS = (ZMQ_HAUSNUMERO + 8);
  ENOTSOCK = (ZMQ_HAUSNUMERO + 9);
  EMSGSIZE = (ZMQ_HAUSNUMERO + 10);
  EAFNOSUPPORT = (ZMQ_HAUSNUMERO + 11);
  ENETUNREACH = (ZMQ_HAUSNUMERO + 12);
  ECONNABORTED = (ZMQ_HAUSNUMERO + 13);
  ECONNRESET = (ZMQ_HAUSNUMERO + 14);
  ENOTCONN = (ZMQ_HAUSNUMERO + 15);
  ETIMEDOUT = (ZMQ_HAUSNUMERO + 16);
  EHOSTUNREACH = (ZMQ_HAUSNUMERO + 17);
  ENETRESET = (ZMQ_HAUSNUMERO + 18);
{*  Native 0MQ error codes.                                                   *}
  EFSM = (ZMQ_HAUSNUMERO + 51);
  ENOCOMPATPROTO = (ZMQ_HAUSNUMERO + 52);
  ETERM = (ZMQ_HAUSNUMERO + 53);
  EMTHREAD = (ZMQ_HAUSNUMERO + 54);
{*  This function retrieves the errno as it is known to 0MQ library. The goal *}
{*  of this function is to make the code 100% portable, including where 0MQ   *}
{*  compiled with certain CRT library (on Windows) is linked to an            *}
{*  application that uses different CRT library.                              *}
function zmq_errno: Integer; cdecl; external libzmq;
{*  Resolves system errors and 0MQ errors to human-readable string.           *}
function zmq_strerror(errnum: Integer):PAnsiChar; cdecl; external libzmq;
{******************************************************************************}
{*  0MQ infrastructure (a.k.a. context) initialisation & termination.         *}
{******************************************************************************}
{$ifdef zmq3}
{*  New API                                                                   *}
{*  Context options                                                           *}
const
  ZMQ_IO_THREADS = 1;
  ZMQ_MAX_SOCKETS = 2;
  ZMQ_SOCKET_LIMIT = 3;
  ZMQ_THREAD_PRIORITY = 3;
  ZMQ_THREAD_SCHED_POLICY = 4;

{*  Default for new contexts                                                  *}
  ZMQ_IO_THREADS_DFLT = 1;
  ZMQ_MAX_SOCKETS_DFLT = 1023;
  ZMQ_THREAD_PRIORITY_DFLT = -1;
  ZMQ_THREAD_SCHED_POLICY_DFLT = -1;

function zmq_ctx_new: Pointer; cdecl; external libzmq;
function zmq_ctx_destroy( context: Pointer ): Integer; cdecl; external libzmq;
function zmq_ctx_set( context: Pointer; option: Integer; optval: Integer ): Integer; cdecl; external libzmq;
function zmq_ctx_get( context: Pointer; option: Integer ): Integer; cdecl; external libzmq;
{$endif}
{$ifdef zmq4}
function zmq_ctx_term (context: Pointer): Integer; cdecl; external libzmq;      //UNTESTED
function zmq_ctx_shutdown (context: Pointer): Integer; cdecl; external libzmq;
{$endif}

{*  Old (legacy) API                                                          *}
function zmq_init(io_threads: Integer): Pointer; cdecl; external libzmq;
function zmq_term(context: Pointer): Integer; cdecl; external libzmq;
{******************************************************************************}
{*  0MQ message definition.                                                   *}
{******************************************************************************}
{$ifdef zmq4}
type
  zmq_msg_t = record
    _: Array[0..64-1] of Byte;
  end;
{$elseif zmq3}
  zmq_msg_t = record
    _: Array[0..32-1] of Byte;
  end;
{$else}
{*  Maximal size of "Very Small Message". VSMs are passed by value            *}
{*  to avoid excessive memory allocation/deallocation.                        *}
{*  If VMSs larger than 255 bytes are required, type of 'vsm_size'            *}
{*  field in zmq_msg_t structure should be modified accordingly.              *}
const
  ZMQ_MAX_VSM_SIZE = 30;
{*  Message types. These integers may be stored in 'content' member of the    *}
{*  message instead of regular pointer to the data.                           *}
  ZMQ_DELIMITER = 31;
  ZMQ_VSM = 32;
{*  Message flags. ZMQ_MSG_SHARED is strictly speaking not a message flag     *}
{*  (it has no equivalent in the wire format), however, making  it a flag     *}
{*  allows us to pack the stucture tigher and thus improve performance.       *}
  ZMQ_MSG_MORE_FLAG = 1;
  ZMQ_MSG_SHARED_FLAG = 128;
  ZMQ_MSG_MASK_FLAG = 129; {* Merges all the flags *}
{*  A message. Note that 'content' is not a pointer to the raw data.          *}
{*  Rather it is pointer to zmq::msg_content_t structure                      *}
{*  (see src/msg_content.hpp for its definition).                             *}
type
  zmq_msg_t = record
    content: Pointer;
    flags: Byte;
    vsm_size: Byte;
    vsm_data: Array[0..ZMQ_MAX_VSM_SIZE-1] of Byte;
  end;
{$endif}
  free_fn = procedure(data, hint: Pointer);
{$ifdef FPC}
  size_t = clong;
{$else}
  size_t = Cardinal;
{$endif}
function zmq_msg_init( var msg: zmq_msg_t ): Integer; cdecl; external libzmq;
function zmq_msg_init_size( var msg: zmq_msg_t; size: size_t ): Integer; cdecl; external libzmq;
function zmq_msg_init_data( var msg: zmq_msg_t; data: Pointer; size: size_t;
  ffn: free_fn; hint: Pointer ): Integer; cdecl; external libzmq;
function zmq_msg_close(var msg: zmq_msg_t): Integer; cdecl; external libzmq;
function zmq_msg_move(dest, src: zmq_msg_t): Integer; cdecl; external libzmq;
function zmq_msg_copy(dest, src: zmq_msg_t): Integer; cdecl; external libzmq;
function zmq_msg_data(var msg: zmq_msg_t): Pointer; cdecl; external libzmq;
function zmq_msg_size(var msg: zmq_msg_t): size_t; cdecl; external libzmq;
{$ifdef zmq3}
function zmq_msg_more (var msg: zmq_msg_t): Integer; cdecl; external libzmq;
function zmq_msg_get (var msg: zmq_msg_t; option: Integer): Integer; cdecl; external libzmq;
function zmq_msg_set (var msg: zmq_msg_t; option: Integer; optval: Integer): Integer; cdecl; external libzmq;
function zmq_msg_send (var msg: zmq_msg_t; s: Pointer; flags: Integer): Integer; cdecl; external libzmq;
function zmq_msg_recv (var msg: zmq_msg_t; s: Pointer; flags: Integer): Integer; cdecl; external libzmq;
{$endif}
{$ifdef zmq4}
//function zmq_msg_gets (var msg: zmq_msg_t_Array; const property: PAnsiChar): PAnsiChar; cdecl; external libzmq;    //Don't think this is tranlated correctly.
{$endif}
{******************************************************************************}
{*  0MQ socket definition.                                                    *}
{******************************************************************************}
{*  Socket types.                                                             *}
const
  ZMQ_PAIR = 0;
  ZMQ_PUB = 1;
  ZMQ_SUB = 2;
  ZMQ_REQ = 3;
  ZMQ_REP = 4;
  ZMQ_DEALER = 5;
  ZMQ_ROUTER = 6;
  ZMQ_PULL = 7;
  ZMQ_PUSH = 8;
  ZMQ_XPUB = 9;
  ZMQ_XSUB = 10;
  ZMQ_XREQ = ZMQ_DEALER;           {*  Old alias, remove in 3.x               *}
  ZMQ_XREP = ZMQ_ROUTER;           {*  Old alias, remove in 3.x               *}
{$ifndef zmq3}
  ZMQ_UPSTREAM = ZMQ_PULL;         {*  Old alias, remove in 3.x               *}
  ZMQ_DOWNSTREAM = ZMQ_PUSH;       {*  Old alias, remove in 3.x               *}
{$endif}
{*  Socket options.                                                           *}
{$ifndef zmq3}
  ZMQ_HWM = 1;
  ZMQ_SWAP = 3;
{$endif}
  ZMQ_AFFINITY = 4;
  ZMQ_IDENTITY = 5;
  ZMQ_SUBSCRIBE = 6;
  ZMQ_UNSUBSCRIBE = 7;
  ZMQ_RATE = 8;
  ZMQ_RECOVERY_IVL = 9;
{$ifndef zmq3}
  ZMQ_MCAST_LOOP = 10;
{$endif}
  ZMQ_SNDBUF = 11;
  ZMQ_RCVBUF = 12;
  ZMQ_RCVMORE = 13;
  ZMQ_FD = 14;
  ZMQ_EVENTS = 15;
  ZMQ_TYPE = 16;
  ZMQ_LINGER = 17;
  ZMQ_RECONNECT_IVL = 18;
  ZMQ_BACKLOG = 19;
{$ifndef zmq3}
  ZMQ_RECOVERY_IVL_MSEC = 20;      {*  opt. recovery time, reconcile in 3.x   *}
{$endif}
  ZMQ_RECONNECT_IVL_MAX = 21;
{$ifdef zmq3}
  ZMQ_MAXMSGSIZE = 22;
  ZMQ_SNDHWM = 23;
  ZMQ_RCVHWM = 24;
  ZMQ_MULTICAST_HOPS = 25;
{$endif}
  ZMQ_RCVTIMEO = 27;
  ZMQ_SNDTIMEO = 28;
{$ifdef zmq3}
  ZMQ_IPV4ONLY = 31;
  ZMQ_LAST_ENDPOINT = 32;
  ZMQ_ROUTER_MANDATORY = 33;
  ZMQ_TCP_KEEPALIVE = 34;
  ZMQ_TCP_KEEPALIVE_CNT = 35;
  ZMQ_TCP_KEEPALIVE_IDLE = 36;
  ZMQ_TCP_KEEPALIVE_INTVL = 37;
  ZMQ_TCP_ACCEPT_FILTER = 38;
  ZMQ_IMMEDIATE = 39;
//  ZMQ_DELAY_ATTACH_ON_CONNECT = 39;
  ZMQ_XPUB_VERBOSE = 40;
{$ifdef zmq4}
  ZMQ_ROUTER_RAW = 41;
  ZMQ_IPV6 = 42;
  ZMQ_MECHANISM = 43;
  ZMQ_PLAIN_SERVER = 44;
  ZMQ_PLAIN_USERNAME = 45;
  ZMQ_PLAIN_PASSWORD = 46;
  ZMQ_CURVE_SERVER = 47;
  ZMQ_CURVE_PUBLICKEY = 48;
  ZMQ_CURVE_SECRETKEY = 49;
  ZMQ_CURVE_SERVERKEY = 50;
  ZMQ_PROBE_ROUTER = 51;
  ZMQ_REQ_CORRELATE = 52;
  ZMQ_REQ_RELAXED = 53;
  ZMQ_CONFLATE = 54;
  ZMQ_ZAP_DOMAIN = 55;
  ZMQ_ROUTER_HANDOVER = 56;
  ZMQ_TOS = 57;
  ZMQ_CONNECT_RID = 61;
  ZMQ_GSSAPI_SERVER = 62;
  ZMQ_GSSAPI_PRINCIPAL = 63;
  ZMQ_GSSAPI_SERVICE_PRINCIPAL = 64;
  ZMQ_GSSAPI_PLAINTEXT = 65;
  ZMQ_HANDSHAKE_IVL = 66;
  ZMQ_SOCKS_PROXY = 68;
  ZMQ_XPUB_NODROP = 69;
{$endif}
{*  Message options                                                           *}
  ZMQ_MORE = 1;
  ZMQ_SRCFD = 2;
  ZMQ_SHARED = 3;
{$endif}
{*  Send/recv options.                                                        *}
{$ifdef zmq3}
  ZMQ_DONTWAIT = 1;
{$else}
  ZMQ_NOBLOCK = 1;
{$endif}
  ZMQ_SNDMORE = 2;
{$ifdef zmq4}
(*  Security mechanisms                                                       *)
  ZMQ_NULL = 0;
  ZMQ_PLAIN = 1;
  ZMQ_CURVE = 2;
  ZMQ_GSSAPI = 3;

(*  Deprecated options and aliases                                            *)
  ZMQ_IPC_FILTER_PID = 58;
  ZMQ_IPC_FILTER_UID = 59;
  ZMQ_IPC_FILTER_GID = 60;
  ZMQ_DELAY_ATTACH_ON_CONNECT = ZMQ_IMMEDIATE;
  ZMQ_NOBLOCK = ZMQ_DONTWAIT;
  ZMQ_FAIL_UNROUTABLE = ZMQ_ROUTER_MANDATORY;
  ZMQ_ROUTER_BEHAVIOR = ZMQ_ROUTER_MANDATORY;

{$endif}
{$ifdef zmq3}
{******************************************************************************}
{*  0MQ socket events and monitoring                                          *}
{******************************************************************************}
{*  Socket transport events (tcp and ipc only)                                *}
  ZMQ_EVENT_CONNECTED = 1;
  ZMQ_EVENT_CONNECT_DELAYED = 2;
  ZMQ_EVENT_CONNECT_RETRIED = 4;
  ZMQ_EVENT_LISTENING = 8;
  ZMQ_EVENT_BIND_FAILED = 16;
  ZMQ_EVENT_ACCEPTED = 32;
  ZMQ_EVENT_ACCEPT_FAILED = 64;
  ZMQ_EVENT_CLOSED = 128;
  ZMQ_EVENT_CLOSE_FAILED = 256;
  ZMQ_EVENT_DISCONNECTED =512;
{$ifdef zmq4}
  ZMQ_EVENT_MONITOR_STOPPED = 1024;
{$endif}

  ZMQ_EVENT_ALL =
    ZMQ_EVENT_CONNECTED or
    ZMQ_EVENT_CONNECT_DELAYED or
    ZMQ_EVENT_CONNECT_RETRIED or
    ZMQ_EVENT_LISTENING or
    ZMQ_EVENT_BIND_FAILED or
    ZMQ_EVENT_ACCEPTED or
    ZMQ_EVENT_ACCEPT_FAILED or
    ZMQ_EVENT_CLOSED or
    ZMQ_EVENT_CLOSE_FAILED or
    ZMQ_EVENT_DISCONNECTED
{$ifdef zmq4}
    or ZMQ_EVENT_MONITOR_STOPPED
{$endif} ;
{*  Socket event data (union member per event)                                *}
type
  zmq_event_t = record
    event: Integer;
    addr: PAnsiChar;
    case Integer of
      0, // connected
      3, // listening
      5, // accepted
      7, // closed
      9: // disconnected
        (
        fd: Integer;
        );
      1, // connect_delayed
      4, // bind_failed
      6, // accept_failed
      8: // close_failed
        (
        err: Integer;
        );
      2: ( //connect_retried
        interval: Integer;
        );
  end;
{$endif}
function zmq_socket(context: Pointer; stype: Integer): Pointer; cdecl; external libzmq;
function zmq_close(s: Pointer): Integer; cdecl; external libzmq;
function zmq_setsockopt(s: Pointer; option: Integer; optval: Pointer; optvallen: size_t ): Integer; cdecl; external libzmq;
function zmq_getsockopt(s: Pointer; option: Integer; optval: Pointer; var optvallen: size_t): Integer; cdecl; external libzmq;
function zmq_bind(s: Pointer; addr: PAnsiChar): Integer; cdecl; external libzmq;
function zmq_connect(s: Pointer; addr: PAnsiChar): Integer; cdecl; external libzmq;
{$ifdef zmq3}
function zmq_unbind(s: Pointer; addr: PAnsiChar): Integer; cdecl; external libzmq;
function zmq_disconnect(s: Pointer; addr: PAnsiChar): Integer; cdecl; external libzmq;
{$endif}
{$ifdef zmq3}
function zmq_send (s: Pointer; const buffer; len: size_t; flags: Integer): Integer; cdecl; external libzmq;
function zmq_recv (s: Pointer; var buffer; len: size_t; flags: Integer): Integer; cdecl; external libzmq;
 {$ifdef zmp4}
 function zmq_send_const (s: Pointer; const buffer; len: size_t; flags: Integer): Integer; cdecl; external libzmq;  //UNTESTED
 {$endif}
{$else}
function zmq_send (s: Pointer; var msg: zmq_msg_t; flags: Integer): Integer; cdecl; external libzmq;
function zmq_recv (s: Pointer; var msg: zmq_msg_t; flags: Integer): Integer; cdecl; external libzmq;
{$endif}
{$ifdef zmq3}
function zmq_sendmsg(s: Pointer; var msg: zmq_msg_t; flags: Integer): Integer; cdecl; external libzmq;
function zmq_recvmsg(s: Pointer; var msg: zmq_msg_t; flags: Integer): Integer; cdecl; external libzmq;
function zmq_socket_monitor( s: Pointer; addr: PAnsiChar; events: Integer ): Integer; cdecl; external libzmq;
{
/*  Experimental                                                              */
struct iovec;
ZMQ_EXPORT int zmq_sendiov (void *s, struct iovec *iov, size_t count, int flags);
ZMQ_EXPORT int zmq_recviov (void *s, struct iovec *iov, size_t *count, int flags);
}
{$endif}
{******************************************************************************}
{*  I/O multiplexing.                                                         *}
{******************************************************************************}
const
  ZMQ_POLLIN = 1;
  ZMQ_POLLOUT = 2;
  ZMQ_POLLERR = 4;
type
  pollitem_t = record
    socket: Pointer;
    fd: Integer; // TSocket???
    events: Word;
    revents: Word;
  end;
function zmq_poll( var items: pollitem_t; nitems: Integer; timeout: Longint ): Integer; cdecl; external libzmq;
{******************************************************************************}
{*  Built-in devices                                                          *}
{******************************************************************************}
{$ifdef zmq3}
{*  Built-in message proxy (3-way) *}
function zmq_proxy( frontend, backend, capture: Pointer ): Integer; cdecl; external libzmq;
 {$ifdef zmq4}
 function zmq_proxy_steerable( frontend, backend, capture, control: Pointer ): Integer; cdecl; external libzmq;   //UNTESTED
 {$endif}
{$endif}

{*  Deprecated aliases *}
const
  ZMQ_STREAMER = 1;
  ZMQ_FORWARDER = 2;
  ZMQ_QUEUE = 3;

(*******************************************************************************
/*  Encryption functions                                                      */
/******************************************************************************)

//  Encode data with Z85 encoding. Returns encoded data                       */
function zmq_z85_encode (dest: pchar; const data: pointer; size: size_t): PAnsiChar; cdecl; external libzmq; //UNTESTED

//  Decode data with Z85 encoding. Returns decoded data                       */
function zmq_z85_decode (dest: pointer; const text: pchar): Byte; cdecl; external libzmq;                     //UNTESTED

//  Generate z85-encoded public and private keypair with libsodium.           */
//  Returns 0 on success.                                                     */
function zmq_curve_keypair (z85_public_key: pAnsiChar; z85_secret_key:pAnsiChar): Integer; cdecl; external libzmq; //UNTESTED


{*  Deprecated method *}
function zmq_device(device: Integer; insocket,outsocket: Pointer): Integer; cdecl; external libzmq;
{*  Helper functions are used by perf tests so that they don't have to care   *}
{*  about minutiae of time-related functions on different OS platforms.       *}
{*  Starts the stopwatch. Returns the handle to the watch.                    *}
function zmq_stopwatch_start: Pointer; stdcall; external libzmq;
{*  Stops the stopwatch. Returns the number of microseconds elapsed since     *}
{*  the stopwatch was started.                                                *}
function zmq_stopwatch_stop( watch: Pointer ): LongWord; stdcall; external libzmq;
{*  Sleeps for specified number of seconds.                                   *}
procedure zmq_sleep( seconds: Integer ); stdcall; external libzmq;
implementation
end.
