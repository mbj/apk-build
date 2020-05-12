# apk-build

Utilities to build an apk overlay.

## Usage

Add the following public key to `/etc/apk/keys/mbj@schirp-dso.com-5e5c5d2b.rsa.pub`

```
-----BEGIN PUBLIC KEY-----
MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAtZRLJDokvEpadk3M1KqW
hJ3sMVJzmP1XNMKsG/PxnfWaYGpGzPlkAgSKbHmbn+McWL6/B2GwhwqO4YCZ02rV
P9BBrzlnTak6OFHaxj9nOB0YV0uXMJWW5foNsmmNhPCDzbLDP/F7HmRcuBiosucb
Xiw1JxuRF99tQeksoMxn4jaqIRLpZr2u2QHGU3SAw9FkL9uHtF3h3GE13sgjWXYO
w+ST3GtURxI6RdL/2L09ShCxt2NvwBNvevNxoZOaCMgu/7c+DnIw7q4yII083XjZ
RKgPgxSylguY+X3uuPaV9ZIX8hCuAuFF1fzbTvl/plyeptB9HF6vtXe4CbsZvdYU
9QIDAQAB
-----END PUBLIC KEY-----
```

Add the repository:

```
echo '@mbj https://mbj-apk.s3.dualstack.us-east-1.amazonaws.com' >> /etc/apk/repositories
```

Add packages from this repository:

```
apk add stack@mbj
```

Supported packages:

* [cache-s3@mbj=0.1.5-r0](https://github.com/fpco/cache-s3)
* [libpq@mbj=12.2](https://www.postgresql.org/) (suitable for static linking)
* [stack@mbj=2.1.3-r0](https://docs.haskellstack.org/en/stable/README/)

## Stability

This is an early experiment. Stability is not guaranteed.

I expect I'll be able to never remove / overwrite packages, and only ever add to the
repository.

Compatibility with alpine releases only is guarded by package specific dependencies.

## S3 Bucket

S3 Bucket is sponsored by [Schirp DSO](https://schirp-dso.com), my company.
Please do your best to cache packages locally. I reserve the right to deny access to
abusers or remove the S3 Bucket any time should hosting it become too expensive.
