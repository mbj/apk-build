FROM alpine:3.14

ENV HOME=/opt/build

RUN apk add                              \
  --no-cache                             \
  --                                     \
  alpine-sdk                             \
  apk-tools                              \
  && addgroup root abuild                \
  && ln -s /var/cache/apk /etc/apk/cache

RUN echo $'-----BEGIN PUBLIC KEY-----\n\
MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAoWXZ75rvmn8I5GoTmxsA\n\
LGv5eWrNf/lrOeiPAoux3H2nCwO0+Qd/Fd0u453B8yq5pmcuyebYvjSle4vH0C7m\n\
g1x2PN57sZLawJbO47ywKw7yN5iWhLTq8R8Zbou9m7o0fSuukGPKzleOFn2MxLw3\n\
0uqcv45Vcsw2msgwfXluycjyCWfoV+dvCeu1pqVLLaG8FP108Pn2yn7IzIvt5abd\n\
pg+6jh4W8t1P2LZ3D0YI8P3+0+d9tcVQe2TAaNFjALWB6GY0D9vncVI+WjPi2NxE\n\
ZHUir6fe0rAx7hLAhyo9SPSCeOQhO8oBijoIE3C5d+cZebUNNuhPGZwKRPmeKDOj\n\
hQIDAQAB\n\
-----END PUBLIC KEY-----'\
>> /etc/apk/keys/mbj.rsa.pub
