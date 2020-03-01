FROM alpine:3.11

ENV HOME=/opt/build

RUN apk add                              \
  --no-cache                             \
  --                                     \
  alpine-sdk                             \
  apk-tools                              \
  && addgroup root abuild                \
  && ln -s /var/cache/apk /etc/apk/cache

RUN echo $'-----BEGIN PUBLIC KEY-----\n\
MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAtZRLJDokvEpadk3M1KqW\n\
hJ3sMVJzmP1XNMKsG/PxnfWaYGpGzPlkAgSKbHmbn+McWL6/B2GwhwqO4YCZ02rV\n\
P9BBrzlnTak6OFHaxj9nOB0YV0uXMJWW5foNsmmNhPCDzbLDP/F7HmRcuBiosucb\n\
Xiw1JxuRF99tQeksoMxn4jaqIRLpZr2u2QHGU3SAw9FkL9uHtF3h3GE13sgjWXYO\n\
w+ST3GtURxI6RdL/2L09ShCxt2NvwBNvevNxoZOaCMgu/7c+DnIw7q4yII083XjZ\n\
RKgPgxSylguY+X3uuPaV9ZIX8hCuAuFF1fzbTvl/plyeptB9HF6vtXe4CbsZvdYU\n\
9QIDAQAB\n\
-----END PUBLIC KEY-----\n'\
>> /etc/apk/keys/mbj@schirp-dso.com-5e5c5d2b.rsa.pub
