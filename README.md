# dnsrest-proxy

`dnsrest-proxy` is a small UDP DNS proxy for networks where classic DNS traffic is filtered, redirected, or unreliable. It listens like a normal DNS server, forwards each query to a configurable DNS-over-HTTPS endpoint, and returns the DNS wire-format response to the original client.

The project originally talked to an early REST-style DNS API. It now uses standard DoH with `application/dns-message`.

## Why

Some networks force all UDP/53 traffic through their own resolvers. That can break privacy, reliability, or access to ordinary domains. Running this proxy locally lets clients continue speaking normal DNS while the upstream lookup travels over HTTPS.

## Features

- Listens for UDP DNS queries on the local `domain` service port.
- Sends upstream queries with DNS-over-HTTPS `POST`.
- Uses Cloudflare DoH by default.
- Accepts any compatible DoH endpoint URL from the command line.
- Preserves the client DNS message ID in downstream responses.
- Includes a lightweight Cabal test suite for config and DoH message handling.

## Build

```sh
cabal build DnsProxy
```

## Run

Use the default Cloudflare endpoint:

```sh
cabal run DnsProxy
```

Use a custom DoH endpoint:

```sh
cabal run DnsProxy -- https://dns.google/dns-query
```

If you run the built executable directly:

```sh
./DnsProxy https://dns.google/dns-query
```

## Test

```sh
cabal test
```

The tests cover command-line config parsing and the DNS message ID normalization/restoration used around DoH requests.

## How It Works

```text
DNS client
    |
    | UDP DNS query
    v
dnsrest-proxy
    |
    | HTTPS POST application/dns-message
    v
DoH resolver
```

Before sending a query upstream, the proxy normalizes the DNS message ID to `0`, which is conventional for DoH. When the DoH response returns, the proxy restores the original client message ID before replying over UDP.

## Dependencies

The executable uses:

- `dns`
- `network`
- `http-client`
- `http-client-tls`
- `bytestring`

The test suite is intentionally small and only depends on `base` and `dns`.
