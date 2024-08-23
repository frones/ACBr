FROM php:7.3-fpm-buster as base
LABEL maintainer="Rafael Dias"

RUN echo "deb http://deb.debian.org/debian buster contrib\n" >> /etc/apt/sources.list &&\
    apt-get update && apt-get install -y --no-install-recommends \
    xvfb \
    openssl \    
    libxml2 \
    libgtk2.0-0 \
    libssl-dev \
    curl \
    libcurl4-gnutls-dev \
    libicu-dev \
    libmcrypt4 \    
    ttf-mscorefonts-installer && \
    ln -s libxml2.so.2 libxml2.so

ENV DISPLAY :99

#LIBS EXTRAS
RUN docker-php-ext-install bcmath
RUN apt-get install -y libbz2-dev
RUN docker-php-ext-install bz2
RUN docker-php-ext-install mbstring
RUN apt-get install -y libpq-dev
RUN apt-get install -y libicu-dev
RUN docker-php-ext-install intl

#GD
RUN apt-get install -y libfreetype6-dev libjpeg62-turbo-dev libpng16-16
RUN docker-php-ext-configure gd --with-freetype-dir=/usr/include/ --with-jpeg-dir=/usr/include/
RUN docker-php-ext-install gd

#PDO - CUSTOMIZAR A SEU DISPOR
#RUN docker-php-ext-install pdo_mysql

EXPOSE 9000

FROM debian:buster-slim as build
WORKDIR /src
COPY ["*.h", "*.cpp", "Makefile", "acbrlibnfephp.ini", "/src/"]
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    php7.3-dev \
    git && \
    rm -rf /var/lib/apt/lists/* && \
    git -c http.sslVerify=false clone https://github.com/CopernicaMarketingSoftware/PHP-CPP.git PHP-CPP --branch v2.2.0 --depth 1 && \
    cd PHP-CPP && make && make install && \
    cd /src/ && make

FROM base As final
WORKDIR /app
COPY --from=build /src/PHP-CPP/libphpcpp.so.2.2.0 /usr/lib/libphpcpp.so.2.2
COPY --from=build /src/acbrlibnfephp.so /usr/local/lib/php/extensions/no-debug-non-zts-20180731/
COPY --from=build /src/acbrlibnfephp.ini /etc/php/7.3/mods-available
COPY libacbrnfe64.so /usr/lib
COPY start.sh /app
RUN docker-php-ext-enable acbrlibnfephp.so
RUN chmod 0755 /app/start.sh
CMD ["bash", "/app/start.sh"]