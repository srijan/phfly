# syntax = docker/dockerfile:1.3
FROM erlang:25-alpine as builder

WORKDIR /app/src
ENV REBAR_BASE_DIR /app/_build

# RUN apk add --no-cache git

COPY rebar.config rebar.lock .
RUN rebar3 compile

FROM builder as prod_compiled

RUN rebar3 as prod compile

FROM prod_compiled as releaser

# create the directory to unpack the release to
RUN mkdir -p /opt/rel

RUN apk add --no-cache tar

RUN --mount=target=. \
    rebar3 as prod tar && \
    tar -zxvf $REBAR_BASE_DIR/prod/rel/*/*.tar.gz -C /opt/rel

FROM alpine:3.16

WORKDIR /opt/phfly
RUN apk add --no-cache openssl ncurses libstdc++ libgcc
COPY --from=releaser /opt/rel .

ENTRYPOINT ["/opt/phfly/bin/phfly"]
CMD ["foreground"]

EXPOSE 8080
